package sgrv.be.auth

import sgrv.be.BackendCapabilities
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, LogoutEvent, RequestContext, SessionNotifier}
import sgrv.be.sessions.WorkoutSessions
import zio.{Cause, Clock, ZIO}
import zio.http.{Header, Method, Response, Routes, Status, handler}

/** Logs out either the whole account or only the device making the request.
  *
  * `/logout` preserves the deliberately strong account-wide behavior: it closes an open workout, revokes the Google
  * grant, and invalidates every signed-in device. `/logout/device` invalidates only the credential in that request and
  * leaves the Google grant and all other credentials alone. If that credential owns the counter, its workout is closed
  * and the role released; logging out a dashboard does not disturb the counter on another device.
  */
object Logout extends BackendPlugin:
  type Requires = AppleOAuth & GoogleOAuth & SessionStore & SessionNotifier & WorkoutSessions

  override val id = "auth-logout"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.appleOAuth) ++
      CapabilitySet.one(BackendCapabilities.googleOAuth) ++
      CapabilitySet.one(BackendCapabilities.sessionStore) ++
      CapabilitySet.one(BackendCapabilities.sessionNotifier) ++
      CapabilitySet.one(BackendCapabilities.workoutSessions)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(
      Method.POST / "logout" -> handler(logoutAccount),
      Method.POST / "logout" / "device" -> handler(logoutDevice)
    )

  private sealed trait LogoutFailure:
    def cause: Throwable

  private final case class ProviderRevocationFailed(provider: IdentityProvider, cause: Throwable) extends LogoutFailure
  private final case class WorkoutClosureFailed(cause: Throwable) extends LogoutFailure
  private final case class SessionInvalidationFailed(cause: Throwable) extends LogoutFailure

  private def logoutAccount: ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO
      .service[RequestContext]
      .flatMap:
        case RequestContext.Authenticated(request, user) =>
          SessionAuth.sessionKey(request) match
            case None             => ZIO.succeed(Response.status(Status.Unauthorized))
            case Some(sessionKey) =>
              for
                secure <- GoogleOAuth.callbackIsSecure
                now <- Clock.instant
                response <- invalidate(user, now).foldZIO(
                  failure => failureResponse(failure),
                  _ =>
                    // Raised only once the workout is closed and every application session is really gone, so a
                    // listener never records an ending that did not happen. Listener failures are isolated inside the
                    // notifier.
                    for _ <- SessionNotifier.loggedOut(LogoutEvent(sessionKey, user, now))
                    yield Response
                      .status(Status.NoContent)
                      .addCookie(Callback.clearedSessionCookie(secure))
                      .addCookie(Callback.clearedStateCookie(secure))
                )
              yield noStore(response)
        case RequestContext.Public(_) => ZIO.succeed(noStore(Response.status(Status.InternalServerError)))

  private def logoutDevice: ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO
      .service[RequestContext]
      .flatMap:
        case RequestContext.Authenticated(request, user) =>
          SessionAuth.sessionKey(request) match
            case None             => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))
            case Some(sessionKey) =>
              for
                secure <- GoogleOAuth.callbackIsSecure
                now <- Clock.instant
                response <- (WorkoutSessions.closeOnDeviceLogout(user.account, sessionKey, now) *>
                  SessionStore.invalidate(sessionKey))
                  .foldZIO(
                    error =>
                      ZIO.logErrorCause(
                        "Could not close and invalidate the current device session",
                        Cause.fail(error)
                      ) *>
                        ZIO.succeed(
                          Response
                            .text("Could not log out this device. Try again.")
                            .status(Status.ServiceUnavailable)
                        ),
                    _ =>
                      SessionNotifier
                        .loggedOut(LogoutEvent(sessionKey, user, now))
                        .as(
                          Response
                            .status(Status.NoContent)
                            .addCookie(Callback.clearedSessionCookie(secure))
                            .addCookie(Callback.clearedStateCookie(secure))
                        )
                  )
              yield noStore(response)
        case RequestContext.Public(_) => ZIO.succeed(noStore(Response.status(Status.InternalServerError)))

  private def invalidate(user: SessionUser, now: java.time.Instant): ZIO[Requires, LogoutFailure, Unit] =
    WorkoutSessions.closeOnLogout(user.account, now).mapError(WorkoutClosureFailed.apply) *>
      revokeProvider(user).mapError(ProviderRevocationFailed(user.identityProvider, _)) *>
      SessionStore.invalidateAll(user.account).mapError(SessionInvalidationFailed.apply)

  private def revokeProvider(user: SessionUser): ZIO[Requires, Throwable, Unit] = user.identityProvider match
    case IdentityProvider.Google =>
      user.refreshToken
        .orElse(user.accessTokenForRevocation)
        .fold[ZIO[GoogleOAuth, Throwable, Unit]](ZIO.unit)(GoogleOAuth.revoke)
    case IdentityProvider.Apple =>
      (user.refreshToken, user.providerClientId) match
        case (Some(token), Some(clientId)) => AppleOAuth.revoke(token, clientId)
        case _                             => ZIO.unit
    case IdentityProvider.Invitation => ZIO.unit

  private def failureResponse(failure: LogoutFailure): ZIO[Any, Nothing, Response] =
    failure match
      case ProviderRevocationFailed(provider, error) =>
        val name = provider.toString
        ZIO.logErrorCause(s"Could not revoke $name authorization during logout", Cause.fail(error)) *>
          ZIO.succeed(Response.text(s"Could not revoke $name authorization. Try again.").status(Status.BadGateway))
      case WorkoutClosureFailed(error) =>
        ZIO.logErrorCause("Could not close the active workout during logout", Cause.fail(error)) *>
          ZIO.succeed(Response.text("Could not close the active workout. Try again.").status(Status.ServiceUnavailable))
      case SessionInvalidationFailed(error) =>
        ZIO.logErrorCause("Could not invalidate every account session during logout", Cause.fail(error)) *>
          ZIO.succeed(
            Response.text("Could not invalidate every signed-in device. Try again.").status(Status.ServiceUnavailable)
          )

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)
