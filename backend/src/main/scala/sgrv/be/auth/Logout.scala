package sgrv.be.auth

import sgrv.be.BackendCapabilities
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, ZIO}
import zio.http.{Header, Method, Response, Routes, Status, handler}

/** Revokes the signed-in user's Google grant and invalidates the opaque browser session. */
object Logout extends BackendPlugin:
  type Requires = GoogleOAuth & SessionStore

  override val id = "auth-logout"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.googleOAuth) ++
      CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.POST / "logout" -> handler(apply))

  private sealed trait LogoutFailure:
    def cause: Throwable

  private final case class GoogleRevocationFailed(cause: Throwable) extends LogoutFailure
  private final case class SessionInvalidationFailed(cause: Throwable) extends LogoutFailure

  private def apply: ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO.service[RequestContext].flatMap:
      case RequestContext.Authenticated(request, user) =>
        request.cookie(Callback.sessionCookieName).map(_.content).filter(_.nonEmpty) match
          case None             => ZIO.succeed(Response.status(Status.Unauthorized))
          case Some(sessionKey) =>
            for
              secure <- GoogleOAuth.callbackIsSecure
              response <- invalidate(sessionKey, user).foldZIO(
                failure => failureResponse(failure),
                _ =>
                  ZIO.succeed(
                    Response
                      .status(Status.NoContent)
                      .addCookie(Callback.clearedSessionCookie(secure))
                      .addCookie(Callback.clearedStateCookie(secure))
                  )
              )
            yield noStore(response)
      case RequestContext.Public(_) => ZIO.succeed(noStore(Response.status(Status.InternalServerError)))

  private def invalidate(sessionKey: String, user: SessionUser): ZIO[Requires, LogoutFailure, Unit] =
    user.refreshToken
      .orElse(user.accessTokenForRevocation)
      .fold[ZIO[GoogleOAuth, Throwable, Unit]](ZIO.unit)(GoogleOAuth.revoke)
      .mapError(GoogleRevocationFailed.apply) *>
      SessionStore.invalidate(sessionKey).mapError(SessionInvalidationFailed.apply)

  private def failureResponse(failure: LogoutFailure): ZIO[Any, Nothing, Response] =
    failure match
      case GoogleRevocationFailed(error) =>
        ZIO.logErrorCause("Could not revoke Google authorization during logout", Cause.fail(error)) *>
          ZIO.succeed(Response.text("Could not revoke Google authorization. Try again.").status(Status.BadGateway))
      case SessionInvalidationFailed(error) =>
        ZIO.logErrorCause("Could not invalidate the browser session during logout", Cause.fail(error)) *>
          ZIO.succeed(Response.text("Could not invalidate the browser session. Try again.").status(Status.ServiceUnavailable))

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)
