package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import sgrv.api.RepProgress
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, Clock, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

/** Accepts what the counting device has measured so far into the account's session.
  *
  * Which session is written to comes from who is signed in, never from the request body: a client can report its count,
  * but not choose whose count it is. And only the device the account's record names as its counter may write: any other
  * is answered with a conflict, which is how a device that has been taken over learns it.
  *
  * The route is deliberately cheap and frequent. Beyond keeping the workout's record current for the history, a request
  * every ten seconds is what keeps a scale-to-zero Cloud Run instance from being reclaimed underneath an acquirer that
  * is mid-workout and, from the platform's point of view, idle.
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

  /** Nobody is signed in. The only failure here that may answer 401: the frontend's HTTP boundary reads that status as
    * the session having gone, and ends it everywhere in the app. A report that arrives a moment after the account
    * stopped counting is not that, and answering 401 to it signed people out of a perfectly good login.
    */
  private case object Unauthenticated extends ProgressFailure

  /** Signed in, but the account has nothing counting: it was never opened, or it has since closed. Either way this
    * device is no longer the counter and should stand down, which is what a conflict says.
    */
  private case object NotCounting extends ProgressFailure
  private case object Displaced extends ProgressFailure
  private final case class Malformed(details: String) extends ProgressFailure
  private final case class WriteFailed(cause: Throwable) extends ProgressFailure

  private def apply(request: Request): ZIO[Requires & RequestContext, Nothing, Response] =
    record(request).foldZIO(failureResponse, _ => ZIO.succeed(noStore(Response.status(Status.NoContent))))

  private def record(request: Request): ZIO[Requires & RequestContext, ProgressFailure, Unit] =
    for
      // The session belongs to the account, not to this browser: which record is written comes from who is signed in.
      email <- ZIO.serviceWithZIO[RequestContext] {
        case RequestContext.Authenticated(_, user) => ZIO.succeed(user.email)
        case _                                     => ZIO.fail(Unauthenticated)
      }
      // Which browser session is reporting, so a device that has been taken over can be told.
      mine <- ZIO.fromOption(CountingSessionListener.browserSession(request)).orElseFail(NotCounting)
      body <- request.body.asString.mapError(error => Malformed(describe(error)))
      progress <- ZIO.fromEither(CountingSessionProgress.reported(body)).mapError(Malformed.apply)
      firestore <- ZIO.service[Firestore]
      account <- AccountKey.of(email).someOrFail(NotCounting)
      keeper <- AccountSessions.store(firestore)
      now <- Clock.instant
      recorded <- keeper
        .counted(account, mine, progress.reps, progress.cadenceSum, progress.elapsedSeconds, now)
        .mapError(WriteFailed.apply)
      // The ownership check and progress write happen in the same transaction. Another device taking over between a
      // read and this write therefore cannot let a displaced counter file one last report.
      _ <- ZIO.fail(Displaced).when(recorded.isEmpty)
    yield ()

  /** The count a request body reports, or why it does not report one.
    *
    * A negative total is rejected rather than clamped: nothing legitimate produces one, so it means the caller and this
    * route disagree about something, and quietly storing a zero would hide that.
    */
  private[sessions] def reported(body: String): Either[String, RepProgress] =
    body
      .fromJson[RepProgress]
      .flatMap(validate)

  private[sessions] def validate(progress: RepProgress): Either[String, RepProgress] =
    if progress.reps < 0 then Left(s"Negative rep count ${progress.reps}")
    else if !progress.cadenceSum.isFinite || progress.cadenceSum < 0 then
      Left(s"Cadence sum ${progress.cadenceSum} is not a number at or above zero")
    else if !progress.elapsedSeconds.isFinite || progress.elapsedSeconds < 0 then
      Left(s"Elapsed ${progress.elapsedSeconds} is not a duration")
    else Right(progress)

  /** The count alone, which is all most readers want of a report. */
  private[sessions] def reps(body: String): Either[String, Int] = reported(body).map(_.reps)

  private def failureResponse(failure: ProgressFailure): ZIO[Any, Nothing, Response] =
    failure match
      case Unauthenticated => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))
      // Conflict rather than an error: the report was well formed and the device was entitled to send it a moment
      // ago. It simply no longer holds the role, and this is how it learns.
      case NotCounting | Displaced => ZIO.succeed(noStore(Response.status(Status.Conflict)))
      case Malformed(details)      =>
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
