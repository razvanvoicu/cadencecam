package sgrv.be.sessions

import com.google.cloud.Timestamp
import com.google.cloud.firestore.{DocumentReference, Firestore}
import java.time.{Duration, Instant}
import scala.jdk.CollectionConverters.*
import sgrv.api.AccountSettings
import sgrv.be.store.GoogleFuture
import zio.{Clock, Task, ZIO}
import zio.json.*

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

  /** What the account has set, as the JSON the two ends already agree on.
    *
    * One string rather than a nest of fields, for the reason the traces are: nothing is ever queried by a weight or a
    * factor, and a document that named each of them would have every exercise's every field indexed for the benefit of
    * no reader at all.
    */
  val settings = "settings"

private[sessions] enum SessionEnd:
  case LoggedOut, Idle, TakenOver

private[sessions] object AccountSessions:
  val idleVariable = "COUNTING_IDLE_MINUTES"

  /** How long a session may go without a rep before it is closed. Ten minutes: long enough to set up between suites and
    * to argue with a camera, short enough that a session left open overnight is not still open in the morning.
    */
  val defaultIdle: Duration = Duration.ofMinutes(10)

  /** Whether a session has gone quiet for longer than it is allowed to.
    *
    * Reads when a rep was last counted, not when the device last spoke: a phone on the bench between suites is idle
    * however many readings its socket is sending, and that is the state this is meant to end.
    */
  def goneQuiet(lastRepAt: Option[Instant], now: Instant, idleAfter: Duration): Boolean =
    lastRepAt.exists(quiet => quiet.isBefore(now.minus(idleAfter)))

  /** The account's store, with the configured timeout. */
  def store(firestore: Firestore): ZIO[Any, Nothing, AccountSessionStore] =
    idleAfter.map(AccountSessionStore(firestore, _))

  /** The account document's name and the session in progress, or nothing when neither is available.
    *
    * One place where "which session am I writing to" is answered, so no route has to work it out from a cookie: the
    * session belongs to the account, and the account is whoever is signed in.
    */
  def active(firestore: Firestore, email: String): ZIO[Any, Throwable, Option[(String, String)]] =
    AccountKey
      .of(email)
      .flatMap:
        case None       => ZIO.none
        case Some(name) =>
          for
            keeper <- store(firestore)
            now <- Clock.instant
            state <- keeper.state(name, now)
          yield state.active.map(name -> _)

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
      _ <- GoogleFuture.fromApiFuture(
        account(name).collection(AccountSchema.sessions).document(session).create(opened.asJava)
      )
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
        account(name)
          .collection(AccountSchema.sessions)
          .document(session)
          .set(moved.asJava, com.google.cloud.firestore.SetOptions.merge())
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
          .set(
            Map[String, AnyRef]("reps" -> java.lang.Long.valueOf(reps.toLong), "repsAt" -> stamp(now)).asJava,
            com.google.cloud.firestore.SetOptions.merge()
          )
      )
      _ <- GoogleFuture.fromApiFuture(
        account(name).set(
          Map[String, AnyRef](AccountSchema.lastRepAt -> stamp(now)).asJava,
          com.google.cloud.firestore.SetOptions.merge()
        )
      )
    yield ()

  /** What the account has set, or nothing when it has never said.
    *
    * Unreadable settings are treated as absent rather than as a failure: the shape can only have changed because this
    * app changed it, and refusing to open a dashboard over a field it no longer understands would be a worse outcome
    * than starting again from the defaults.
    */
  def settings(name: String): Task[Option[AccountSettings]] =
    GoogleFuture
      .fromApiFuture(account(name).get())
      .flatMap: snapshot =>
        Option
          .when(snapshot.exists)(Option(snapshot.getString(AccountSchema.settings)))
          .flatten
          .fold(ZIO.none): stored =>
            stored.fromJson[AccountSettings] match
              case Right(settings) => ZIO.some(settings)
              case Left(details)   =>
                ZIO.logWarning(s"Ignoring unreadable account settings: $details").as(None)

  def saveSettings(name: String, settings: AccountSettings): Task[Unit] =
    GoogleFuture
      .fromApiFuture(
        account(name).set(
          Map[String, AnyRef](AccountSchema.settings -> settings.toJson).asJava,
          com.google.cloud.firestore.SetOptions.merge()
        )
      )
      .unit

  /** Files a captured recording under the session that produced it. */
  def recordTrace(name: String, session: String, traceId: String, fields: Map[String, AnyRef]): Task[Unit] =
    GoogleFuture
      .fromApiFuture(
        account(name)
          .collection(AccountSchema.sessions)
          .document(session)
          .collection("traces")
          .document(traceId)
          .create(fields.asJava)
      )
      .unit

  /** Files one step of a peer exchange under the session the two devices are pairing for.
    *
    * Under the session rather than the account, so an exchange cannot outlive what it was pairing for: when the session
    * closes, the offers and candidates that belonged to it go with it.
    */
  def postSignal(
      name: String,
      session: String,
      from: String,
      kind: String,
      body: String,
      peer: String,
      at: Instant
  ): Task[Unit] =
    val fields = Map[String, AnyRef](
      "postedAt" -> stamp(at),
      "from" -> from,
      "kind" -> kind,
      "body" -> body,
      // Which watching device this belongs to. A counter may be watched by several at once, and an offer, an answer
      // and a set of candidates belong to one pair of ends rather than to the account.
      "peer" -> peer
    )
    GoogleFuture
      .fromApiFuture(
        account(name)
          .collection(AccountSchema.sessions)
          .document(session)
          .collection("signals")
          .document()
          .create(fields.asJava)
      )
      .unit

  /** Everything the other end has filed since the cursor, oldest first, with the position to ask from next time.
    *
    * Anything older than a couple of minutes is passed over: a link forms in seconds, so an older offer belongs to an
    * attempt both ends have given up on, and answering one is worse than not answering.
    */
  def signalsFor(
      name: String,
      session: String,
      mine: String,
      since: Option[Instant],
      now: Instant
  ): Task[(Seq[(String, String, String)], String)] =
    val floor = since.getOrElse(now.minusSeconds(120))
    val query = account(name)
      .collection(AccountSchema.sessions)
      .document(session)
      .collection("signals")
      .whereGreaterThan("postedAt", stamp(floor))
      .orderBy("postedAt", com.google.cloud.firestore.Query.Direction.ASCENDING)
      .limit(64)
    GoogleFuture
      .fromApiFuture(query.get())
      .map: found =>
        val documents = found.getDocuments.asScala.toSeq
        val theirs = documents.filter(document => document.getString("from") != mine).map { document =>
          (
            Option(document.getString("kind")).getOrElse(""),
            Option(document.getString("body")).getOrElse(""),
            Option(document.getString("peer")).getOrElse("")
          )
        }
        // Only past what is being handed over. Taking the newest of everything -- this end's own filings included --
        // advances the cursor past the other end's answer whenever the two are written in the same instant, and that
        // answer is then never handed on: the exchange stalls with both ends believing they have spoken.
        val consumed = documents
          .filter(document => document.getString("from") != mine)
          .flatMap(document => Option(document.getTimestamp("postedAt")).map(_.toDate.toInstant))
          .maxOption
        (theirs, consumed.getOrElse(floor).toString)

  /** Ends the session and leaves it as a record. The account keeps its document; only the pointer is cleared. */
  def close(name: String, session: String, why: SessionEnd, now: Instant): Task[Unit] =
    val ended = Map[String, AnyRef](
      AccountSchema.completedAt -> stamp(now),
      AccountSchema.endedBy -> why.toString
    )
    for
      _ <- GoogleFuture
        .fromApiFuture(
          account(name)
            .collection(AccountSchema.sessions)
            .document(session)
            .set(ended.asJava, com.google.cloud.firestore.SetOptions.merge())
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
