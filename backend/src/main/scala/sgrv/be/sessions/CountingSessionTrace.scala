package sgrv.be.sessions

import com.google.cloud.Timestamp
import com.google.cloud.firestore.Firestore
import java.security.SecureRandom
import java.time.Instant
import sgrv.api.SignalTrace
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, Clock, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

/** How a recording is stored: one document in the `traces` collection under the workout it was captured in.
  *
  * The field names are the recording's own, so the test that every field of [[SignalTrace]] is written can be a
  * comparison of two sets.
  */
private[sessions] object TraceSchema:
  val capturedAt = "capturedAt"
  val sampleRateHz = "sampleRateHz"
  val samples = "samples"
  val reps = "reps"
  val lock = "lock"
  val note = "note"

  /** What the camera said about itself, and what became of its controls.
    *
    * Stored because they were not: the phones have been sending these since the field existed and every one was dropped
    * here, which turned a question about a device into an absence, and an absence into a wrong conclusion about why a
    * picture was darkening.
    */
  val camera = "camera"
  val device = "device"
  val controls = "controls"
  val controlsAtSample = "controlsAtSample"

  /** A recording as the fields it is filed with.
    *
    * The samples are stored as their JSON text rather than as Firestore arrays. Thousands of numbers would otherwise be
    * indexed element by element, for a document nothing will ever be queried by: the recording is read back whole or
    * not at all.
    */
  def fields(trace: SignalTrace, at: Instant): Map[String, AnyRef] =
    Map[String, AnyRef](
      capturedAt -> Timestamp.ofTimeSecondsAndNanos(at.getEpochSecond, at.getNano),
      sampleRateHz -> java.lang.Double.valueOf(trace.sampleRateHz),
      samples -> trace.samples.toJson,
      reps -> java.lang.Long.valueOf(trace.reps.toLong),
      lock -> trace.lock
    ) ++ trace.note.map(note -> _)
      ++ trace.camera.map(camera -> _)
      ++ trace.device.map(device -> _)
      ++ trace.controls.map(controls -> _)
      ++ trace.controlsAtSample.map(at => controlsAtSample -> java.lang.Long.valueOf(at.toLong))

/** Accepts a captured recording of the acquirer's signals and files it under the session that produced it.
  *
  * Kept for offline work rather than for the app to read back. The detector's thresholds were first chosen by reasoning
  * about signals nobody had recorded, and each guess made that way was wrong in a different direction; a real recording
  * turns a question about behaviour into something that can be replayed rather than argued about.
  */
object CountingSessionTrace extends BackendPlugin:
  type Requires = Firestore & SessionStore

  override val id = "counting-session-trace"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.firestore) ++
      CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.POST / "countingSession" / "trace" -> handler((request: Request) => apply(request)))

  /** The longest recording this app makes is six minutes at ten hertz, 3,600 samples a channel. Anything much longer is
    * not a recording of this app but a mistake or an abuse, and is refused before it reaches Firestore.
    */
  private[sessions] val maximumSamplesPerChannel = 6000

  private sealed trait TraceFailure
  private case object NoSession extends TraceFailure
  private final case class Malformed(details: String) extends TraceFailure
  private final case class WriteFailed(cause: Throwable) extends TraceFailure

  private def apply(request: Request): ZIO[Requires & RequestContext, Nothing, Response] =
    record(request).foldZIO(failureResponse, id => ZIO.succeed(noStore(Response.json(s"""{"traceId":"$id"}"""))))

  private def record(request: Request): ZIO[Requires & RequestContext, TraceFailure, String] =
    for
      email <- ZIO.serviceWithZIO[RequestContext] {
        case RequestContext.Authenticated(_, user) => ZIO.succeed(user.email)
        case _                                     => ZIO.fail(NoSession)
      }
      body <- request.body.asString.mapError(error => Malformed(describe(error)))
      trace <- ZIO.fromEither(CountingSessionTrace.parse(body)).mapError(Malformed.apply)
      firestore <- ZIO.service[Firestore]
      located <- AccountSessions.active(firestore, email).mapError(WriteFailed.apply)
      (account, session) <- ZIO.fromOption(located).orElseFail(NoSession)
      keeper <- AccountSessions.store(firestore)
      now <- Clock.instant
      traceId <- ZIO.succeed(CountingSessionTrace.traceId())
      _ <- keeper
        .recordTrace(account, session, traceId, TraceSchema.fields(trace, now))
        .mapError(WriteFailed.apply)
      _ <- ZIO.logInfo(s"Captured a signal trace of ${trace.samples.size} channels for session $session")
    yield traceId

  /** The recording a request body carries, or why it does not carry one. */
  private[sessions] def parse(body: String): Either[String, SignalTrace] =
    body
      .fromJson[SignalTrace]
      .flatMap: trace =>
        val longest = trace.samples.values.map(_.size).maxOption.getOrElse(0)
        if trace.samples.isEmpty then Left("A trace with no channels records nothing")
        else if longest > maximumSamplesPerChannel then
          Left(s"A channel of $longest samples is longer than any recording this app makes")
        else if trace.sampleRateHz <= 0 then Left(s"Nonsensical sample rate ${trace.sampleRateHz}")
        else Right(trace)

  /** Opaque and unguessable, so one session's recordings cannot be enumerated from another's. */
  private[sessions] def traceId(): String =
    val bytes = new Array[Byte](12)
    SecureRandom().nextBytes(bytes)
    bytes.map(byte => f"${byte & 0xff}%02x").mkString

  private def failureResponse(failure: TraceFailure): ZIO[Any, Nothing, Response] =
    failure match
      case NoSession          => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))
      case Malformed(details) =>
        ZIO.logWarning(s"Rejected a signal trace: $details") *>
          ZIO.succeed(noStore(Response.status(Status.BadRequest)))
      case WriteFailed(error) =>
        ZIO.logWarningCause("Could not store a signal trace", Cause.fail(error)) *>
          ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)

  private def describe(error: Throwable): String =
    Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
