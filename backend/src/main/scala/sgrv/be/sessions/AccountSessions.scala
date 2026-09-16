package sgrv.be.sessions

import com.google.cloud.Timestamp
import com.google.cloud.firestore.{DocumentReference, Firestore}
import java.time.{Duration, Instant}
import scala.jdk.CollectionConverters.*
import sgrv.be.store.GoogleFuture
import zio.{Task, ZIO}

/** One account, and the counting sessions it has held.
  *
  * The account document is named by a keyed digest of the email, and carries only which session is in progress and when
  * a rep was last counted. The sessions themselves are a subcollection and are kept after they close: a finished set is
  * the record worth having, and only its successor is live.
  */
private[sessions] object AccountSchema:
  val collection = "CountingSessions"
  val sessions = "sessions"

  /** The session in progress, absent when the account has none. One field, so "one active session per account" is a
    * property of the shape rather than something that has to be enforced by a query.
    */
  val activeSession = "activeSession"

  /** When a rep was last counted, which is what the idle timeout reads. Traffic does not touch it: a phone left on the
    * bench between suites is idle however many readings its socket is sending.
    */
  val lastRepAt = "lastRepAt"
  val startedAt = "startedAt"
  val completedAt = "completedAt"

  /** Why a session ended: the operator logged out, it went quiet, or another device took the role. */
  val endedBy = "endedBy"

  /** Which browser session is counting, so a takeover is a change of hand rather than a new session. */
  val counter = "counter"

private[sessions] enum SessionEnd:
  case LoggedOut, Idle, TakenOver

private[sessions] object AccountSessions:
  val idleVariable = "COUNTING_IDLE_MINUTES"

  /** How long a session may go without a rep before it is closed. Ten minutes: long enough to set up between suites
    * and to argue with a camera, short enough that a session left open overnight is not still open in the morning.
    */
  val defaultIdle: Duration = Duration.ofMinutes(10)

  /** Whether a session has gone quiet for longer than it is allowed to.
    *
    * Reads when a rep was last counted, not when the device last spoke: a phone on the bench between suites is idle
    * however many readings its socket is sending, and that is the state this is meant to end.
    */
  def goneQuiet(lastRepAt: Option[Instant], now: Instant, idleAfter: Duration): Boolean =
    lastRepAt.exists(quiet => quiet.isBefore(now.minus(idleAfter)))

  /** The configured timeout, or the default when it is unset or unreadable. */
  def idleAfter: ZIO[Any, Nothing, Duration] =
    zio.System
      .env(idleVariable)
      .catchAll(_ => ZIO.succeed(None))
      .map: configured =>
        configured
          .flatMap(value => scala.util.Try(value.trim.toLong).toOption)
          .filter(_ > 0)
          .fold(defaultIdle)(Duration.ofMinutes)

/** What a device is told when it arrives: whether this account already has something counting. */
private[sessions] final case class AccountState(active: Option[String], counter: Option[String])

private[sessions] final class AccountSessionStore(firestore: Firestore, idleAfter: Duration):

  private def account(name: String): DocumentReference =
    firestore.collection(AccountSchema.collection).document(name)

  private def stamp(at: Instant) = Timestamp.ofTimeSecondsAndNanos(at.getEpochSecond, at.getNano)

  /** What is counting for this account, closing a session first if it has gone quiet for longer than the timeout.
    *
    * Evaluated when someone asks rather than swept by a timer: an idle instance runs no code, so a background job would
    * need scheduling, while every question that matters arrives as a request anyway.
    */
  def state(name: String, now: Instant): Task[AccountState] =
    GoogleFuture
      .fromApiFuture(account(name).get())
      .flatMap: snapshot =>
        if !snapshot.exists then ZIO.succeed(AccountState(None, None))
        else
          val active = Option(snapshot.getString(AccountSchema.activeSession))
          val counter = Option(snapshot.getString(AccountSchema.counter))
          val quietSince = Option(snapshot.getTimestamp(AccountSchema.lastRepAt)).map(_.toDate.toInstant)
          active match
            case Some(session) if AccountSessions.goneQuiet(quietSince, now, idleAfter) =>
              close(name, session, SessionEnd.Idle, now).as(AccountState(None, None))
            case other => ZIO.succeed(AccountState(other, counter))

  /** Takes the counter's role: joins the session in progress, or opens one when there is none. */
  def takeCounting(name: String, email: String, browserSession: String, now: Instant): Task[String] =
    state(name, now).flatMap: current =>
      current.active match
        case Some(session) => hand(name, session, browserSession, now).as(session)
        case None          => start(name, email, browserSession, now)

  private def start(name: String, email: String, browserSession: String, now: Instant): Task[String] =
    val session = f"${now.toEpochMilli}%d-${scala.util.Random.nextInt(0x1000)}%03x"
    val opened = Map[String, AnyRef](
      AccountSchema.startedAt -> stamp(now),
      AccountSchema.counter -> browserSession
    )
    for
      _ <- GoogleFuture.fromApiFuture(account(name).collection(AccountSchema.sessions).document(session).create(opened.asJava))
      _ <- GoogleFuture.fromApiFuture(
        account(name).set(
          Map[String, AnyRef](
            AccountSchema.activeSession -> session,
            AccountSchema.counter -> browserSession,
            AccountSchema.lastRepAt -> stamp(now)
          ).asJava,
          com.google.cloud.firestore.SetOptions.merge()
        )
      )
    yield session

  /** Moves the counter's role to another device without ending the session. */
  private def hand(name: String, session: String, browserSession: String, now: Instant): Task[Unit] =
    val moved = Map[String, AnyRef](AccountSchema.counter -> browserSession)
    for
      _ <- GoogleFuture.fromApiFuture(
        account(name).collection(AccountSchema.sessions).document(session).set(moved.asJava, com.google.cloud.firestore.SetOptions.merge())
      )
      _ <- GoogleFuture.fromApiFuture(account(name).set(moved.asJava, com.google.cloud.firestore.SetOptions.merge()))
    yield ()

  /** Records a rep count, which is also what keeps the session from going idle. */
  def counted(name: String, session: String, reps: Int, now: Instant): Task[Unit] =
    for
      _ <- GoogleFuture.fromApiFuture(
        account(name)
          .collection(AccountSchema.sessions)
          .document(session)
          .set(Map[String, AnyRef]("reps" -> java.lang.Long.valueOf(reps.toLong), "repsAt" -> stamp(now)).asJava,
            com.google.cloud.firestore.SetOptions.merge())
      )
      _ <- GoogleFuture.fromApiFuture(
        account(name).set(Map[String, AnyRef](AccountSchema.lastRepAt -> stamp(now)).asJava, com.google.cloud.firestore.SetOptions.merge())
      )
    yield ()

  /** Ends the session and leaves it as a record. The account keeps its document; only the pointer is cleared. */
  def close(name: String, session: String, why: SessionEnd, now: Instant): Task[Unit] =
    val ended = Map[String, AnyRef](
      AccountSchema.completedAt -> stamp(now),
      AccountSchema.endedBy -> why.toString
    )
    for
      _ <- GoogleFuture
        .fromApiFuture(
          account(name).collection(AccountSchema.sessions).document(session).set(ended.asJava, com.google.cloud.firestore.SetOptions.merge())
        )
        .unit
      _ <- GoogleFuture
        .fromApiFuture(
          account(name).update(
            Map[String, AnyRef](
              AccountSchema.activeSession -> com.google.cloud.firestore.FieldValue.delete(),
              AccountSchema.counter -> com.google.cloud.firestore.FieldValue.delete()
            ).asJava
          )
        )
        .unit
    yield ()
