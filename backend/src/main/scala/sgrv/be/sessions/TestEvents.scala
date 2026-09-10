package sgrv.be.sessions

import com.google.cloud.Timestamp
import com.google.cloud.firestore.Firestore
import java.time.Instant
import scala.jdk.CollectionConverters.*
import sgrv.api.TestEvent
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import sgrv.be.store.GoogleFuture
import zio.{Cause, Clock, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

private[sessions] object TestEventSchema:
  val collection = "TestEvents"
  val recordedAt = "recordedAt"
  val userEmail = "userEmail"
  val runId = "runId"
  val testName = "testName"
  val kind = "kind"
  val reference = "reference"
  val acquired = "acquired"
  val lagSeconds = "lagSeconds"
  val atSeconds = "atSeconds"
  val detail = "detail"

/** Records what a test run observed about the counter.
  *
  * Top-level rather than filed under a counting session: a run is about the detector's behaviour rather than about
  * anyone's workout, and the interesting comparisons are between runs.
  */
private[sessions] final class TestEventStore(firestore: Firestore):
  def record(event: TestEvent, email: String, at: Instant): zio.Task[Unit] =
    GoogleFuture
      .fromApiFuture(firestore.collection(TestEventSchema.collection).document().create(fields(event, email, at).asJava))
      .unit

  private def fields(event: TestEvent, email: String, at: Instant): Map[String, AnyRef] =
    Map[String, AnyRef](
      TestEventSchema.recordedAt -> Timestamp.ofTimeSecondsAndNanos(at.getEpochSecond, at.getNano),
      TestEventSchema.userEmail -> email,
      TestEventSchema.runId -> event.runId,
      TestEventSchema.testName -> event.testName,
      TestEventSchema.kind -> event.kind,
      TestEventSchema.reference -> java.lang.Long.valueOf(event.reference.toLong),
      TestEventSchema.acquired -> java.lang.Long.valueOf(event.acquired.toLong),
      TestEventSchema.lagSeconds -> java.lang.Double.valueOf(event.lagSeconds),
      TestEventSchema.atSeconds -> java.lang.Double.valueOf(event.atSeconds)
    ) ++ event.detail.map(TestEventSchema.detail -> _)

/** Accepts observations from a test run and files them.
  *
  * Also logged, at a level that matches what the observation means: a stall or a shortfall is a warning because it is
  * the thing being hunted, while a test starting or finishing is not.
  */
object TestEventReports extends BackendPlugin:
  type Requires = Firestore & SessionStore

  override val id = "test-events"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.firestore) ++ CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.POST / "test" / "event" -> handler((request: Request) => apply(request)))

  private[sessions] val notable = Set("stalled", "recovered", "discrepancy", "failed")

  private def apply(request: Request): ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO.serviceWithZIO[RequestContext]:
      case RequestContext.Authenticated(_, user) => record(request, user.email)
      case _                                     => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))

  private def record(request: Request, email: String): ZIO[Requires, Nothing, Response] =
    val stored =
      for
        body <- request.body.asString
        event <- ZIO.fromEither(body.fromJson[TestEvent]).mapError(details => IllegalArgumentException(details))
        firestore <- ZIO.service[Firestore]
        now <- Clock.instant
        _ <- TestEventStore(firestore).record(event, email, now)
        _ <- log(event)
      yield ()
    stored.foldZIO(
      // Said out loud rather than swallowed: a harness whose observations are being silently rejected would look
      // exactly like a detector with nothing to report.
      error =>
        ZIO.logWarningCause("Rejected a test event", Cause.fail(error)) *>
          ZIO.succeed(noStore(Response.status(Status.BadRequest))),
      _ => ZIO.succeed(noStore(Response.status(Status.NoContent)))
    )

  private def log(event: TestEvent): ZIO[Any, Nothing, Unit] =
    val summary =
      f"[${event.testName}] ${event.kind}: reference ${event.reference}, acquired ${event.acquired}, " +
        f"lag ${event.lagSeconds}%.2fs at ${event.atSeconds}%.1fs${event.detail.fold("")(d => s" -- $d")}"
    if notable(event.kind) then ZIO.logWarning(summary) else ZIO.logInfo(summary)

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)
