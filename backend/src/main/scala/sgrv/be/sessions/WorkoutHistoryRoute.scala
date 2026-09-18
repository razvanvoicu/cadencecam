package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import sgrv.api.{DiscardWorkout, WorkoutHistory}
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

/** The account's own workouts: what it has done, and the throwing away of any one of them.
  *
  * Both scoped to whoever is signed in, and to that account's own subcollection. A workout's id is not a secret -- it
  * travels to the device that is counting into it -- so the account is taken from the session rather than from the
  * request, and an id from one account can no more read than delete another's.
  */
object WorkoutHistoryRoute extends BackendPlugin:
  type Requires = Firestore & SessionStore

  override val id = "workout-history"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.firestore) ++ CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(
      Method.GET / "countingSession" / "history" -> handler(listed),
      Method.POST / "countingSession" / "history" / "discard" -> handler((request: Request) => discard(request))
    )

  private def listed: ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO.serviceWithZIO[RequestContext]:
      case RequestContext.Authenticated(_, user) =>
        val found =
          for
            firestore <- ZIO.service[Firestore]
            keeper <- AccountSessions.store(firestore)
            name <- AccountKey.of(user.email)
            workouts <- name match
              case None        => ZIO.succeed(Seq.empty)
              case Some(named) => keeper.history(named, WorkoutHistory.Limit)
          yield workouts
        found
          .catchAll: error =>
            // An empty history and an unreadable one look the same on screen, and nothing a person does about it
            // differs. The fault is logged where it can be acted on.
            ZIO.logWarningCause("Could not read the account's history", Cause.fail(error)) *> ZIO.succeed(Seq.empty)
          .map(workouts => noStore(Response.json(WorkoutHistory(workouts).toJson)))
      case _ => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))

  private def discard(request: Request): ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO.serviceWithZIO[RequestContext]:
      case RequestContext.Authenticated(_, user) =>
        val removed =
          for
            body <- request.body.asString
            asked <- ZIO.fromEither(body.fromJson[DiscardWorkout]).mapError(IllegalArgumentException(_))
            workout <- ZIO
              .succeed(asked.id.trim)
              .filterOrFail(_.nonEmpty)(IllegalArgumentException("A workout to discard must be named"))
            firestore <- ZIO.service[Firestore]
            keeper <- AccountSessions.store(firestore)
            name <- AccountKey.of(user.email)
            response <- name match
              case None        => ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))
              case Some(named) =>
                keeper.discard(named, workout) *>
                  // Said out loud: deleting somebody's record is worth a line in the log even when they asked for it.
                  ZIO.logWarning(s"Discarded a workout at the account's request") *>
                  ZIO.succeed(noStore(Response.status(Status.NoContent)))
          yield response
        removed.catchAll:
          case invalid: IllegalArgumentException =>
            ZIO.logWarning(s"Refused a workout discard: ${invalid.getMessage}") *>
              ZIO.succeed(noStore(Response.status(Status.BadRequest)))
          case error =>
            ZIO.logWarningCause("Could not discard a workout", Cause.fail(error)) *>
              ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))
      case _ => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)
