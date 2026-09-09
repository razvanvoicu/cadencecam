package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import sgrv.api.RepProgress
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, Clock, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

/** Accepts the acquirer's running total into its counting session.
  *
  * Which session is written to comes from the browser's own session cookie, not from the request body: a client can
  * report its count, but not choose whose count it is.
  *
  * The route is deliberately cheap and frequent. Beyond recording progress for the dashboard to read, a request every
  * few seconds is what keeps a scale-to-zero Cloud Run instance from being reclaimed underneath an acquirer that is
  * mid-workout and, from the platform's point of view, idle.
  */
object CountingSessionProgress extends BackendPlugin:
  type Requires = Firestore & SessionStore

  override val id = "counting-session-progress"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.firestore) ++
      CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.POST / "countingSession" / "reps" -> handler((request: Request) => apply(request)))

  private sealed trait ProgressFailure
  private case object NoSession extends ProgressFailure
  private final case class Malformed(details: String) extends ProgressFailure
  private final case class WriteFailed(cause: Throwable) extends ProgressFailure

  private def apply(request: Request): ZIO[Requires, Nothing, Response] =
    record(request).foldZIO(failureResponse, _ => ZIO.succeed(noStore(Response.status(Status.NoContent))))

  private def record(request: Request): ZIO[Requires, ProgressFailure, Unit] =
    for
      // The access policy has already established that the session is real; this only asks which record it names.
      documentId <- ZIO.fromOption(CountingSessionListener.documentId(request)).orElseFail(NoSession)
      body <- request.body.asString.mapError(error => Malformed(describe(error)))
      reps <- ZIO.fromEither(CountingSessionProgress.reps(body)).mapError(Malformed.apply)
      firestore <- ZIO.service[Firestore]
      now <- Clock.instant
      _ <- CountingSessionStore(firestore).recordProgress(documentId, reps, now).mapError(WriteFailed.apply)
    yield ()

  /** The count a request body reports, or why it does not report one.
    *
    * A negative total is rejected rather than clamped: nothing legitimate produces one, so it means the caller and this
    * route disagree about something, and quietly storing a zero would hide that.
    */
  private[sessions] def reps(body: String): Either[String, Int] =
    body.fromJson[RepProgress].flatMap: progress =>
      Either.cond(progress.reps >= 0, progress.reps, s"Negative rep count ${progress.reps}")

  private def failureResponse(failure: ProgressFailure): ZIO[Any, Nothing, Response] =
    failure match
      case NoSession           => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))
      case Malformed(details)  =>
        ZIO.logWarning(s"Rejected a counting-session progress report: $details") *>
          ZIO.succeed(noStore(Response.status(Status.BadRequest)))
      case WriteFailed(error) =>
        // Logged rather than retried: the acquirer reports again on its next tick, so one lost write costs nothing
        // beyond a slightly stale dashboard.
        ZIO.logWarningCause("Could not record counting-session progress", Cause.fail(error)) *>
          ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)

  private def describe(error: Throwable): String =
    Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
