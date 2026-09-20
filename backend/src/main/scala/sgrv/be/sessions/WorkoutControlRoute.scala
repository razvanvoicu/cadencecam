package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import sgrv.api.{WorkoutAction, WorkoutControl, WorkoutSnapshot, WorkoutState}
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, Clock, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

/** Starts and stops the account's workout at the counter's request.
  *
  * The dashboard reaches this route through the counter over their direct link. That keeps one authority for both the
  * detector and the stored workout: a dashboard cannot accidentally open a session which no camera is counting into.
  */
object WorkoutControlRoute extends BackendPlugin:
  type Requires = Firestore & SessionStore

  override val id = "workout-control"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.firestore) ++ CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.POST / "countingSession" / "control" -> handler((request: Request) => apply(request)))

  private def apply(request: Request): ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO.serviceWithZIO[RequestContext]:
      case RequestContext.Authenticated(_, user) => control(user.email, request)
      case _                                     => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))

  private def control(email: String, request: Request): ZIO[Requires, Nothing, Response] =
    val changed =
      for
        browser <- ZIO
          .fromOption(CountingSessionListener.browserSession(request))
          .orElseFail(IllegalArgumentException("The counter carries no browser session"))
        body <- request.body.asString
        command <- ZIO.fromEither(body.fromJson[WorkoutControl]).mapError(IllegalArgumentException(_))
        snapshot <- ZIO
          .fromOption(command.snapshot)
          .orElseFail(IllegalArgumentException(s"${command.action} requires the workout settings snapshot"))
          .flatMap(value => ZIO.fromEither(validate(value)))
        progress <- command.action match
          case WorkoutAction.Start => ZIO.succeed(None)
          case WorkoutAction.Stop  =>
            ZIO
              .fromOption(command.progress)
              .orElseFail(IllegalArgumentException("Stop requires the final workout measurement"))
              .flatMap(value => ZIO.fromEither(CountingSessionProgress.validate(value)))
              .map(Some(_))
        firestore <- ZIO.service[Firestore]
        account <- AccountKey.of(email).someOrFail(IllegalStateException("The account has no storage key"))
        keeper <- AccountSessions.store(firestore)
        now <- Clock.instant
        session <- command.action match
          case WorkoutAction.Start => keeper.startWorkout(account, browser, snapshot, now)
          case WorkoutAction.Stop  => keeper.stopWorkout(account, browser, progress.get, snapshot, now)
        response <- session match
          case Some(id) =>
            ZIO.logInfo(s"${command.action} workout $id for $email") *>
              ZIO.succeed(noStore(Response.json(WorkoutState(command.action == WorkoutAction.Start).toJson)))
          case None => ZIO.succeed(noStore(Response.status(Status.Conflict)))
      yield response

    changed.catchAll:
      case invalid: IllegalArgumentException =>
        ZIO.logWarning(s"Refused workout control: ${invalid.getMessage}") *>
          ZIO.succeed(noStore(Response.status(Status.BadRequest)))
      case error =>
        ZIO.logWarningCause("Could not change the workout state", Cause.fail(error)) *>
          ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)

  private[sessions] def validate(snapshot: WorkoutSnapshot): Either[String, WorkoutSnapshot] =
    if snapshot.exerciseType.trim.isEmpty then Left("The workout has no exercise type")
    else if !snapshot.calories.isFinite || snapshot.calories < 0 then
      Left(s"Calories ${snapshot.calories} is not a number at or above zero")
    else if !snapshot.exerciseFactor.isFinite || snapshot.exerciseFactor <= 0 then
      Left(s"Exercise factor ${snapshot.exerciseFactor} is not above zero")
    else if !snapshot.weightKilograms.isFinite || snapshot.weightKilograms <= 0 then
      Left(s"Weight ${snapshot.weightKilograms} is not above zero")
    else Right(snapshot.copy(exerciseType = snapshot.exerciseType.trim))
