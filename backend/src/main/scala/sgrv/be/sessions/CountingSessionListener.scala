package sgrv.be.sessions

import com.google.cloud.Timestamp
import com.google.cloud.firestore.Firestore
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.time.Instant
import scala.jdk.CollectionConverters.*
import sgrv.api.{CountingSession, SignalTrace}
import sgrv.be.BackendCapabilities
import sgrv.be.core.{CapabilitySet, CurrentUserContributor, LoginEvent, LogoutEvent, RequestContext, SessionListener}
import sgrv.be.auth.Callback
import sgrv.be.store.GoogleFuture
import zio.http.Request
import zio.{Task, ZIO}
import zio.json.*
import zio.json.ast.Json

private[sessions] object CountingSessionSchema:
  val collection = "CountingSessions"
  val userEmail = "userEmail"
  val startedAt = "startedAt"

  /** Present once the session has ended. Absent means still open, which is what a query for live sessions keys on.
    * Records are never deleted: they are the raw material for later analytics.
    */
  val completedAt = "completedAt"

  /** The acquirer's running total, and when it was last reported. Overwritten rather than appended: the dashboard wants
    * the current count, and the history of how it got there is not what this record is for.
    */
  val reps = "reps"
  val repsAt = "repsAt"

  /** Captured signal recordings, one document each, under the session that produced them.
    *
    * A subcollection rather than fields on the session: a trace is large next to the record it belongs to, there may be
    * many of them, and nothing reading a session's progress should have to carry a minute of samples with it.
    */
  val traces = "traces"
  val capturedAt = "capturedAt"
  val sampleRateHz = "sampleRateHz"
  val samples = "samples"
  val lock = "lock"
  val note = "note"

  /** What the camera said about itself, and what became of its controls.
    *
    * Stored because they were not: the phones have been sending these since the field existed and every one was dropped
    * here, which turned a question about a device into an absence, and an absence into a wrong conclusion about why a
    * picture was darkening.
    */
  val camera = "camera"
  val controls = "controls"
  val controlsAtSample = "controlsAtSample"

/** The listener's private adapter over the host's generic `firestore` capability.
  *
  * A counting session is one login's workout: the record opened when the user signs in, and the place the acquirer's
  * state is meant to be written for the dashboard to read. It is a different thing from the browser session in the
  * `Access` collection, which exists only to authenticate requests.
  */
private[sessions] final class CountingSessionStore(firestore: Firestore):
  private def document(id: String) = firestore.collection(CountingSessionSchema.collection).document(id)

  /** Idempotent per login, so a replayed callback cannot open a second record for the same session. */
  def open(id: String, userEmail: String, startedAt: Instant): Task[Unit] =
    GoogleFuture
      .fromApiFuture(document(id).get())
      .flatMap: snapshot =>
        if snapshot.exists then ZIO.unit
        else GoogleFuture.fromApiFuture(document(id).create(fields(userEmail, startedAt).asJava)).unit

  /** Marks the session ended. Deliberately an update rather than a delete: the record is kept for analytics. Firestore
    * rejects an update to a missing document, which is the right outcome — there is nothing to complete.
    */
  def complete(id: String, completedAt: Instant): Task[Unit] =
    GoogleFuture
      .fromApiFuture(
        document(id).update(Map[String, AnyRef](CountingSessionSchema.completedAt -> stamp(completedAt)).asJava)
      )
      .unit

  /** Records how far the acquirer has counted.
    *
    * An update rather than a merging set, for the same reason as [[complete]]: a report for a session that was never
    * opened is a mistake worth surfacing, not a record worth conjuring.
    */
  def recordProgress(id: String, reps: Int, at: Instant): Task[Unit] =
    GoogleFuture.fromApiFuture(document(id).update(progress(reps, at).asJava)).unit

  /** Files one captured recording under its session.
    *
    * The samples are stored as their JSON text rather than as Firestore arrays. Twenty-four hundred numbers would
    * otherwise be indexed element by element, for a document nothing will ever be queried by: the recording is read
    * back whole or not at all.
    */
  def recordTrace(id: String, traceId: String, trace: SignalTrace, at: Instant): Task[Unit] =
    GoogleFuture
      .fromApiFuture(
        document(id).collection(CountingSessionSchema.traces).document(traceId).create(fields(trace, at).asJava)
      )
      .unit

  private def fields(trace: SignalTrace, at: Instant): Map[String, AnyRef] =
    Map[String, AnyRef](
      CountingSessionSchema.capturedAt -> stamp(at),
      CountingSessionSchema.sampleRateHz -> java.lang.Double.valueOf(trace.sampleRateHz),
      CountingSessionSchema.samples -> trace.samples.toJson,
      CountingSessionSchema.reps -> java.lang.Long.valueOf(trace.reps.toLong),
      CountingSessionSchema.lock -> trace.lock
    ) ++ trace.note.map(CountingSessionSchema.note -> _)
      ++ trace.camera.map(CountingSessionSchema.camera -> _)
      ++ trace.controls.map(CountingSessionSchema.controls -> _)
      ++ trace.controlsAtSample.map(at => CountingSessionSchema.controlsAtSample -> java.lang.Long.valueOf(at.toLong))

  private def progress(reps: Int, at: Instant): Map[String, AnyRef] =
    Map[String, AnyRef](
      // Firestore has one integer type and it is 64-bit; boxing to Long keeps a read back from depending on which
      // numeric type happened to be written.
      CountingSessionSchema.reps -> java.lang.Long.valueOf(reps.toLong),
      CountingSessionSchema.repsAt -> stamp(at)
    )

  private def fields(userEmail: String, startedAt: Instant): Map[String, AnyRef] =
    Map[String, AnyRef](
      CountingSessionSchema.userEmail -> userEmail,
      CountingSessionSchema.startedAt -> stamp(startedAt)
    )

  private def stamp(instant: Instant): Timestamp =
    Timestamp.ofTimeSecondsAndNanos(instant.getEpochSecond, instant.getNano)

/** Opens a counting session whenever a login succeeds.
  *
  * Nothing in the frontend asks for this and no route exposes it: the record exists by the time the browser is
  * redirected home, because the host raises the event inside the OAuth callback.
  */
object CountingSessionListener extends SessionListener:
  type Requires = Firestore

  override val id = "counting-session"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.one(BackendCapabilities.firestore)

  override def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit] =
    for
      firestore <- ZIO.service[Firestore]
      _ <- CountingSessionStore(firestore).open(documentId(event.sessionKey), event.user.email, event.at)
      _ <- ZIO.logInfo(s"Opened counting session for ${event.user.email}")
    yield ()

  override def onLogout(event: LogoutEvent): ZIO[Requires, Throwable, Unit] =
    for
      firestore <- ZIO.service[Firestore]
      _ <- CountingSessionStore(firestore).complete(documentId(event.sessionKey), event.at)
      _ <- ZIO.logInfo(s"Completed counting session for ${event.user.email}")
    yield ()

  /** Derives the record's id from the browser session key by hashing, so one login maps to exactly one counting session
    * without the opaque session key being copied into a second collection.
    */
  /** The counting session id for a request's browser session, or `None` when it carries no session cookie. */
  private[sessions] def documentId(request: Request): Option[String] =
    request.cookie(Callback.sessionCookieName).map(_.content).map(_.trim).filter(_.nonEmpty).map(documentId)

  private[sessions] def documentId(sessionKey: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(sessionKey.getBytes(UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

/** Tells the frontend which counting session the current login belongs to.
  *
  * The id is derived from the browser session key rather than read back from Firestore, so this costs nothing on a page
  * load. It rides along on `/me`, which the frontend already calls on every load, instead of needing a request of its
  * own — and the key it is filed under matches this contributor's id.
  */
object CountingSessionContributor extends CurrentUserContributor:
  type Requires = Any

  override val id = CountingSession.Key
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty

  override def contribute(context: RequestContext.Authenticated): ZIO[Requires, Throwable, Option[Json]] =
    ZIO.succeed:
      CountingSessionListener
        .documentId(context.request)
        .map(sessionId => Json.Obj("sessionId" -> Json.Str(sessionId)))
