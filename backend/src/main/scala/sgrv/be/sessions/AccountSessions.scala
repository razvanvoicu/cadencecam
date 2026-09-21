package sgrv.be.sessions

import com.google.api.core.{ApiFunction, ApiFutures}
import com.google.cloud.Timestamp
import com.google.cloud.firestore.{DocumentReference, DocumentSnapshot, Firestore, Transaction}
import com.google.common.util.concurrent.MoreExecutors
import java.time.{Duration, Instant}
import scala.jdk.CollectionConverters.*
import sgrv.api.{AccountSettings, CountsBy, RepProgress, Workout, WorkoutSnapshot}
import sgrv.be.store.GoogleFuture
import zio.{Clock, Task, ZIO}
import zio.json.*

/** One account, and the counting sessions it has held.
  *
  * The account document is named by a keyed digest of the email (see [[AccountKey]]), and carries which session is in
  * progress, which browser is counting into it, when that browser last reported, and the account's settings. The
  * sessions themselves are a subcollection and are kept after they close: a finished set is the record worth having --
  * it is what the history lists -- and only the one in progress is live.
  *
  * {{{
  *   CountingSessions/{keyed digest of the email}           activeSession, counter, lastRepAt, settings
  *     peerSignals/{auto id}                                one step of a peer exchange
  *     sessions/{session id}                                startedAt, completedAt, endedBy, counter,
  *                                                          reps, repsAt, cadenceSum, elapsedSeconds
  *       traces/{trace id}                                  one captured recording (see TraceSchema)
  * }}}
  */
private[sessions] object AccountSchema:
  val collection = "CountingSessions"
  val sessions = "sessions"

  /** Recordings belong to the workout that produced them. Peer messages live at account level because the dashboard
    * must reach the counter before Start has created a workout.
    */
  val traces = "traces"
  val peerSignals = "peerSignals"
  /** The old per-workout mailbox, retained only so deleting an older workout removes everything beneath it. */
  val signals = "signals"

  /** The session in progress, absent when the account has none. One field, so "one active session per account" is a
    * property of the shape rather than something that has to be enforced by a query.
    */
  val activeSession = "activeSession"

  /** When the counting device last reported, which is what the idle timeout reads.
    *
    * Named for the rep it was meant to date, but moved by every accepted report, and a counter reports every ten
    * seconds whether or not a rep has been counted since. So what closes a session for being idle is a counter that has
    * stopped reporting -- its page closed, its phone asleep -- rather than an exerciser resting in front of one.
    */
  val lastRepAt = "lastRepAt"
  val startedAt = "startedAt"
  val completedAt = "completedAt"

  /** Why a session ended: the operator stopped it, it went quiet, or another device took the role. */
  val endedBy = "endedBy"

  /** Which browser session is counting, so a takeover is a change of hand rather than a new session. */
  val counter = "counter"

  /** What a workout measured, plus the settings and calorie conclusion frozen for its history row. */
  val reps = "reps"
  val repsAt = "repsAt"
  val cadenceSum = "cadenceSum"
  val elapsedSeconds = "elapsedSeconds"
  val exerciseType = "exerciseType"
  val calories = "calories"
  val exerciseFactor = "exerciseFactor"
  val weightKilograms = "weightKilograms"
  val countsBy = "countsBy"

  /** What the account has set, as the JSON the two ends already agree on.
    *
    * One string rather than a nest of fields, for the reason the traces are: nothing is ever queried by a weight or a
    * factor, and a document that named each of them would have every exercise's every field indexed for the benefit of
    * no reader at all.
    */
  val settings = "settings"

private[sessions] enum SessionEnd:
  case Stopped, Idle, TakenOver

private[sessions] object AccountSessions:
  val idleVariable = "COUNTING_IDLE_MINUTES"

  /** How long a session may go without a report before it is closed. Ten minutes: long enough to set up between suites
    * and to argue with a camera, short enough that a session left open overnight is not still open in the morning.
    */
  val defaultIdle: Duration = Duration.ofMinutes(10)

  /** Whether a session has gone quiet for longer than it is allowed to, judged by the mark [[AccountSchema.lastRepAt]]
    * holds. A session that has never been marked has only just opened, and is not closed for it.
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
    accountState(firestore, email).map(_.flatMap((name, state) => state.active.map(name -> _)))

  /** The account record after applying the idle timeout, paired with its opaque document name. */
  def accountState(firestore: Firestore, email: String): ZIO[Any, Throwable, Option[(String, AccountState)]] =
    AccountKey
      .of(email)
      .flatMap:
        case None       => ZIO.none
        case Some(name) =>
          for
            keeper <- store(firestore)
            now <- Clock.instant
            state <- keeper.state(name, now)
          yield Some(name -> state)

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

private[sessions] object AccountSessionStore:
  /** Firestore caps a batch at five hundred; this leaves room and keeps one commit small. */
  val DeleteBatch = 400

  /** One stored session, as the history lists it. Absent fields read as a workout that counted nothing, which is what a
    * session opened and abandoned without a rep actually is.
    */
  private[sessions] def workoutOf(document: com.google.cloud.firestore.DocumentSnapshot): Workout =
    def millis(field: String) = Option(document.getTimestamp(field)).map(_.toDate.toInstant.toEpochMilli.toDouble)
    val snapshot =
      for
        exerciseType <- Option(document.getString(AccountSchema.exerciseType))
        calories <- Option(document.getDouble(AccountSchema.calories)).map(_.doubleValue)
        exerciseFactor <- Option(document.getDouble(AccountSchema.exerciseFactor)).map(_.doubleValue)
        weightKilograms <- Option(document.getDouble(AccountSchema.weightKilograms)).map(_.doubleValue)
        countsBy <- Option(document.getString(AccountSchema.countsBy))
          .flatMap(value => scala.util.Try(CountsBy.valueOf(value)).toOption)
      yield WorkoutSnapshot(exerciseType, calories, exerciseFactor, weightKilograms, countsBy)
    Workout(
      id = document.getId,
      startedAtMillis = millis(AccountSchema.startedAt).getOrElse(0.0),
      endedAtMillis = millis(AccountSchema.completedAt),
      reps = Option(document.getLong(AccountSchema.reps)).fold(0)(_.intValue),
      cadenceSum = Option(document.getDouble(AccountSchema.cadenceSum)).fold(0.0)(_.doubleValue),
      elapsedSeconds = Option(document.getDouble(AccountSchema.elapsedSeconds)).fold(0.0)(_.doubleValue),
      endedBy = Option(document.getString(AccountSchema.endedBy)),
      snapshot = snapshot
    )

private[sessions] final class AccountSessionStore(firestore: Firestore, idleAfter: Duration):

  private def account(name: String): DocumentReference =
    firestore.collection(AccountSchema.collection).document(name)

  private def stamp(at: Instant) = Timestamp.ofTimeSecondsAndNanos(at.getEpochSecond, at.getNano)

  /** Reads an account and decides all writes which depend on it inside one Firestore transaction. Firestore may invoke
    * the callback more than once when another request changes the account concurrently, so callers must derive their
    * answer only from the supplied snapshot and limit their effects to writes on `transaction`.
    */
  private def transact[A](name: String)(
      decide: (Transaction, DocumentReference, DocumentSnapshot) => A
  ): Task[A] =
    val reference = account(name)
    GoogleFuture.fromApiFuture:
      firestore.runAsyncTransaction[A]: transaction =>
        ApiFutures.transform(
          transaction.get(reference),
          new ApiFunction[DocumentSnapshot, A]:
            override def apply(snapshot: DocumentSnapshot): A = decide(transaction, reference, snapshot)
          ,
          MoreExecutors.directExecutor()
        )

  private def stateOf(snapshot: DocumentSnapshot): AccountState =
    if !snapshot.exists then AccountState(None, None)
    else
      AccountState(
        Option(snapshot.getString(AccountSchema.activeSession)),
        Option(snapshot.getString(AccountSchema.counter))
      )

  private def quietSince(snapshot: DocumentSnapshot): Option[Instant] =
    Option(snapshot.getTimestamp(AccountSchema.lastRepAt)).map(_.toDate.toInstant)

  private def end(
      transaction: Transaction,
      reference: DocumentReference,
      session: String,
      why: SessionEnd,
      now: Instant,
      clearCounter: Boolean
  ): Unit =
    val ended = Map[String, AnyRef](
      AccountSchema.completedAt -> stamp(now),
      AccountSchema.endedBy -> why.toString
    )
    val _ = transaction.set(
      reference.collection(AccountSchema.sessions).document(session),
      ended.asJava,
      com.google.cloud.firestore.SetOptions.merge()
    )
    val cleared = Map.newBuilder[String, AnyRef]
    cleared += (AccountSchema.activeSession -> com.google.cloud.firestore.FieldValue.delete())
    cleared += (AccountSchema.lastRepAt -> com.google.cloud.firestore.FieldValue.delete())
    if clearCounter then cleared += (AccountSchema.counter -> com.google.cloud.firestore.FieldValue.delete())
    val _ = transaction.update(reference, cleared.result().asJava)

  /** What is counting for this account, closing a session first if it has gone quiet for longer than the timeout.
    *
    * Evaluated when someone asks rather than swept by a timer: an idle instance runs no code, so a background job would
    * need scheduling, while every question that matters arrives as a request anyway.
    */
  def state(name: String, now: Instant): Task[AccountState] =
    transact(name): (transaction, reference, snapshot) =>
      val current = stateOf(snapshot)
      current.active match
        case Some(session) if AccountSessions.goneQuiet(quietSince(snapshot), now, idleAfter) =>
          end(transaction, reference, session, SessionEnd.Idle, now, clearCounter = true)
          AccountState(None, None)
        case _ => current

  /** Takes the counter's role without starting a workout. If a workout is already active this is a takeover within it,
    * preserving the run while ensuring that only the new browser may report or stop it.
    */
  def takeCounter(name: String, browserSession: String, now: Instant): Task[AccountState] =
    transact(name): (transaction, reference, snapshot) =>
      val before = stateOf(snapshot)
      val active = before.active.filterNot(_ => AccountSessions.goneQuiet(quietSince(snapshot), now, idleAfter))
      before.active.filterNot(active.contains).foreach(session =>
        end(transaction, reference, session, SessionEnd.Idle, now, clearCounter = false)
      )
      val moved = Map[String, AnyRef](AccountSchema.counter -> browserSession)
      val _ = transaction.set(reference, moved.asJava, com.google.cloud.firestore.SetOptions.merge())
      active.foreach: session =>
        val _ = transaction.set(
          reference.collection(AccountSchema.sessions).document(session),
          moved.asJava,
          com.google.cloud.firestore.SetOptions.merge()
        )
      AccountState(active, Some(browserSession))

  /** Opens a workout for the browser currently holding the counter role. Repeated Start requests are idempotent. */
  def startWorkout(
      name: String,
      browserSession: String,
      workoutSnapshot: WorkoutSnapshot,
      now: Instant
  ): Task[Option[String]] =
    // Chosen outside the callback because Firestore may retry a transaction. Every attempt must create the same
    // session rather than leaving the caller with whichever random id the final attempt happened to choose.
    val newSession = f"${now.toEpochMilli}%d-${scala.util.Random.nextInt(0x1000)}%03x"
    transact(name): (transaction, reference, snapshot) =>
      val current = stateOf(snapshot)
      Option.when(current.counter.contains(browserSession)):
        current.active.filterNot(_ => AccountSessions.goneQuiet(quietSince(snapshot), now, idleAfter)) match
          case Some(session) => session
          case None          =>
            current.active.foreach(session =>
              end(transaction, reference, session, SessionEnd.Idle, now, clearCounter = false)
            )
            val opened = Map[String, AnyRef](
              AccountSchema.startedAt -> stamp(now),
              AccountSchema.counter -> browserSession
            ) ++ snapshotFields(workoutSnapshot)
            val _ = transaction.create(
              reference.collection(AccountSchema.sessions).document(newSession),
              opened.asJava
            )
            val _ = transaction.set(
              reference,
              Map[String, AnyRef](
                AccountSchema.activeSession -> newSession,
                AccountSchema.lastRepAt -> stamp(now)
              ).asJava,
              com.google.cloud.firestore.SetOptions.merge()
            )
            newSession

  private def progressFields(progress: RepProgress, now: Instant): Map[String, AnyRef] =
    Map[String, AnyRef](
      AccountSchema.reps -> java.lang.Long.valueOf(progress.reps.toLong),
      AccountSchema.repsAt -> stamp(now),
      AccountSchema.cadenceSum -> java.lang.Double.valueOf(progress.cadenceSum),
      AccountSchema.elapsedSeconds -> java.lang.Double.valueOf(progress.elapsedSeconds)
    ) ++ progress.calories.map(value => AccountSchema.calories -> java.lang.Double.valueOf(value))

  private def snapshotFields(snapshot: WorkoutSnapshot): Map[String, AnyRef] =
    Map[String, AnyRef](
      AccountSchema.exerciseType -> snapshot.exerciseType,
      AccountSchema.calories -> java.lang.Double.valueOf(snapshot.calories),
      AccountSchema.exerciseFactor -> java.lang.Double.valueOf(snapshot.exerciseFactor),
      AccountSchema.weightKilograms -> java.lang.Double.valueOf(snapshot.weightKilograms),
      AccountSchema.countsBy -> snapshot.countsBy.toString
    )

  /** Writes the final measurement and closes the active workout in the same transaction. The counter role remains so
    * either screen can start the next workout without re-entering or re-pairing.
    */
  def stopWorkout(
      name: String,
      browserSession: String,
      progress: RepProgress,
      workoutSnapshot: WorkoutSnapshot,
      now: Instant
  ): Task[Option[String]] =
    transact(name): (transaction, reference, snapshot) =>
      val current = stateOf(snapshot)
      current.active
        .filter(_ => current.counter.contains(browserSession))
        .map: session =>
          val _ = transaction.set(
            reference.collection(AccountSchema.sessions).document(session),
            (progressFields(progress, now) ++ snapshotFields(workoutSnapshot)).asJava,
            com.google.cloud.firestore.SetOptions.merge()
          )
          end(transaction, reference, session, SessionEnd.Stopped, now, clearCounter = false)
          session

  /** Records what a workout has measured so far. Also moves the account's idle mark, whatever the count: see
    * [[AccountSchema.lastRepAt]].
    */
  def counted(
      name: String,
      browserSession: String,
      progress: RepProgress,
      now: Instant
  ): Task[Option[String]] =
    transact(name): (transaction, reference, snapshot) =>
      val current = stateOf(snapshot)
      current.active
        .filter(_ => current.counter.contains(browserSession))
        .map: session =>
          val _ = transaction.set(
            reference.collection(AccountSchema.sessions).document(session),
            progressFields(progress, now).asJava,
            com.google.cloud.firestore.SetOptions.merge()
          )
          val _ = transaction.set(
            reference,
            Map[String, AnyRef](AccountSchema.lastRepAt -> stamp(now)).asJava,
            com.google.cloud.firestore.SetOptions.merge()
          )
          session

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

  /** The account's workouts, newest first.
    *
    * Read straight from the sessions subcollection rather than from a query across accounts: an account's own sessions
    * are its own subcollection, so this needs no index and can see nobody else's.
    */
  def history(name: String, limit: Int): Task[Seq[Workout]] =
    val query = account(name)
      .collection(AccountSchema.sessions)
      .orderBy(AccountSchema.startedAt, com.google.cloud.firestore.Query.Direction.DESCENDING)
      .limit(limit)
    GoogleFuture
      .fromApiFuture(query.get())
      .map(_.getDocuments.asScala.toSeq.map(AccountSessionStore.workoutOf))

  /** Throws one workout away, with everything filed under it.
    *
    * The recordings and the pairing messages go with it: they belong to that workout and to nothing else, and a
    * deletion that left them behind would be a deletion in name only. Firestore does not remove a document's
    * subcollections when the document goes, so they are removed first and by hand.
    */
  def discard(name: String, session: String): Task[Unit] =
    val workout = account(name).collection(AccountSchema.sessions).document(session)
    for
      _ <- forget(workout)
      // If the account was counting into it, it no longer is: a pointer to a workout that has gone would leave the
      // next device believing something was already counting and refusing to start.
      snapshot <- GoogleFuture.fromApiFuture(account(name).get())
      counting = Option.when(snapshot.exists)(Option(snapshot.getString(AccountSchema.activeSession))).flatten
      _ <- ZIO
        .when(counting.contains(session)):
          GoogleFuture.fromApiFuture(
            account(name).update(
              Map[String, AnyRef](
                AccountSchema.activeSession -> com.google.cloud.firestore.FieldValue.delete(),
                AccountSchema.lastRepAt -> com.google.cloud.firestore.FieldValue.delete()
              ).asJava
            )
          )
        .unit
    yield ()

  /** Removes one workout with everything filed under it. Firestore leaves a document's subcollections behind when the
    * document goes, so they are emptied first and by hand.
    */
  private def forget(workout: com.google.cloud.firestore.DocumentReference): Task[Unit] =
    ZIO.foreachDiscard(Seq(AccountSchema.traces, AccountSchema.signals))(under =>
      deleteAll(workout.collection(under))
    ) *>
      GoogleFuture.fromApiFuture(workout.delete()).unit

  /** Everything this account has: its workouts, its settings, and the test results filed under its address.
    *
    * The account document goes last. It holds the pointer to whatever was counting, so while it stands the app still
    * behaves as though there were a session; removing it first and then failing halfway would leave the workouts
    * orphaned under a name nothing refers to any more.
    */
  def forgetEverything(name: String, email: String): Task[Unit] =
    val sessions = account(name).collection(AccountSchema.sessions)
    def nextPage: Task[Unit] =
      GoogleFuture
        .fromApiFuture(sessions.limit(AccountSessionStore.DeleteBatch).get())
        .flatMap: found =>
          val documents = found.getDocuments.asScala.toSeq
          if documents.isEmpty then ZIO.unit
          else ZIO.foreachDiscard(documents)(document => forget(document.getReference)) *> nextPage
    for
      _ <- nextPage
      _ <- deleteAll(account(name).collection(AccountSchema.peerSignals))
      _ <- GoogleFuture.fromApiFuture(account(name).delete())
      _ <- deleteAll(
        firestore
          .collection(TestEventSchema.collection)
          .whereEqualTo(TestEventSchema.userEmail, email)
      )
    yield ()

  /** Empties a query's results, in batches, because Firestore has no recursive delete from a client library. */
  private def deleteAll(collection: com.google.cloud.firestore.Query): Task[Unit] =
    GoogleFuture
      .fromApiFuture(collection.limit(AccountSessionStore.DeleteBatch).get())
      .flatMap: found =>
        val documents = found.getDocuments.asScala.toSeq
        if documents.isEmpty then ZIO.unit
        else
          val batch = firestore.batch()
          documents.foreach(document => { val _ = batch.delete(document.getReference) })
          GoogleFuture.fromApiFuture(batch.commit()) *> deleteAll(collection)

  /** Files a captured recording under the session that produced it. */
  def recordTrace(name: String, session: String, traceId: String, fields: Map[String, AnyRef]): Task[Unit] =
    GoogleFuture
      .fromApiFuture(
        account(name)
          .collection(AccountSchema.sessions)
          .document(session)
          .collection(AccountSchema.traces)
          .document(traceId)
          .create(fields.asJava)
      )
      .unit

  /** Files one step of a peer exchange under the account. Pairing has to precede Start so a dashboard can ask the
    * counter to create the workout; the two-minute read window keeps abandoned exchanges from being reused.
    */
  def postSignal(
      name: String,
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
          .collection(AccountSchema.peerSignals)
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
      mine: String,
      since: Option[Instant],
      now: Instant
  ): Task[(Seq[(String, String, String)], String)] =
    val floor = since.getOrElse(now.minusSeconds(120))
    val query = account(name)
      .collection(AccountSchema.peerSignals)
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

  /** Ends whichever session is active and leaves it as a record. The decision and both document changes are one
    * transaction, so a concurrent takeover cannot leave the account pointing at a completed session or clear a newer
    * one. A session already quiet is still recorded as idle rather than as the later event which happened to notice it.
    */
  def close(name: String, why: SessionEnd, now: Instant): Task[Option[String]] =
    transact(name): (transaction, reference, snapshot) =>
      stateOf(snapshot).active.map: session =>
        val actual =
          if AccountSessions.goneQuiet(quietSince(snapshot), now, idleAfter) then SessionEnd.Idle else why
        end(transaction, reference, session, actual, now, clearCounter = true)
        session
