package sgrv.be.auth

import sgrv.be.BackendCapabilities
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, Clock, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}

/** Renews a recoverable browser session after validating its stored Google refresh token. */
object RefreshSession extends BackendPlugin:
  type Requires = GoogleOAuth & SessionStore

  override val id = "auth-refresh-session"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.googleOAuth) ++
      CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.POST / "refreshSession" -> handler((request: Request) => apply(request)))

  private[auth] val expiresAtHeader = "X-Session-Expires-At"

  private sealed trait RefreshFailure
  private case object NotRenewable extends RefreshFailure
  private final case class GoogleRefreshFailed(cause: Throwable) extends RefreshFailure
  private final case class SessionStoreFailed(operation: String, cause: Throwable) extends RefreshFailure

  private def apply(request: Request): ZIO[Requires, Nothing, Response] =
    renew(request).foldZIO(failureResponse, response => ZIO.succeed(noStore(response)))

  private def renew(request: Request): ZIO[Requires, RefreshFailure, Response] =
    for
      sessionKey <- ZIO
        .fromOption(request.cookie(Callback.sessionCookieName).map(_.content).filter(_.nonEmpty))
        .orElseFail(NotRenewable)
      user <- SessionStore
        .findForRefresh(sessionKey)
        .mapError(SessionStoreFailed("reading", _))
        .someOrFail(NotRenewable)
      refreshToken <- ZIO.fromOption(user.refreshToken).orElseFail(NotRenewable)
      _ <- GoogleOAuth.accessToken(refreshToken).mapError(GoogleRefreshFailed.apply)
      now <- Clock.instant
      expiresAt = now.plus(Callback.sessionLifetime)
      _ <- SessionStore
        .renew(sessionKey, expiresAt)
        .mapError(SessionStoreFailed("renewing", _))
      secure <- GoogleOAuth.callbackIsSecure
    yield Response
      .json(Me.json(user))
      .addHeader(Header.Custom(expiresAtHeader, expiresAt.toString))
      .addCookie(Callback.sessionCookie(sessionKey, secure))

  private def failureResponse(failure: RefreshFailure): ZIO[Any, Nothing, Response] =
    failure match
      case NotRenewable               => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))
      case GoogleRefreshFailed(error) =>
        ZIO.logWarningCause("Could not renew the Google-backed browser session", Cause.fail(error)) *>
          ZIO.succeed(noStore(Response.status(Status.Unauthorized)))
      case SessionStoreFailed(operation, error) =>
        ZIO.logErrorCause(s"Could not renew the browser session while $operation Firestore", Cause.fail(error)) *>
          ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)
