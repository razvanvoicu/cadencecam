package sgrv.be.auth

import sgrv.be.BackendCapabilities
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, Clock, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}

/** Renews a still-active browser session after validating its stored Google refresh token.
  *
  * An expired session is not resurrected. A browser which slept past expiry must return to Login, just like a native
  * app, and the active lookup also performs the Counter/workout cleanup associated with that expiry.
  */
object RefreshSession extends BackendPlugin:
  type Requires = AppleOAuth & GoogleOAuth & InvitationAccounts & SessionStore

  override val id = "auth-refresh-session"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.appleOAuth) ++
      CapabilitySet.one(BackendCapabilities.googleOAuth) ++
      CapabilitySet.one(BackendCapabilities.invitationAccounts) ++
      CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.POST / "refreshSession" -> handler((request: Request) => apply(request)))

  private[auth] val expiresAtHeader = "X-Session-Expires-At"

  private sealed trait RefreshFailure
  private case object NotRenewable extends RefreshFailure
  private final case class IdentityValidationFailed(provider: IdentityProvider, cause: Throwable) extends RefreshFailure
  private final case class SessionStoreFailed(operation: String, cause: Throwable) extends RefreshFailure

  private def apply(request: Request): ZIO[Requires, Nothing, Response] =
    renew(request).foldZIO(failureResponse, response => ZIO.succeed(noStore(response)))

  private def renew(request: Request): ZIO[Requires, RefreshFailure, Response] =
    for
      sessionKey <- ZIO
        .fromOption(request.cookie(Callback.sessionCookieName).map(_.content).filter(_.nonEmpty))
        .orElseFail(NotRenewable)
      now <- Clock.instant
      user <- SessionStore
        .find(sessionKey, now)
        .mapError(SessionStoreFailed("reading", _))
        .someOrFail(NotRenewable)
      _ <- validateIdentity(user)
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
      case NotRenewable                              => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))
      case IdentityValidationFailed(provider, error) =>
        ZIO.logWarningCause(
          s"Could not renew the ${provider.toString.toLowerCase}-backed browser session",
          Cause.fail(error)
        ) *>
          ZIO.succeed(noStore(Response.status(Status.Unauthorized)))
      case SessionStoreFailed(operation, error) =>
        ZIO.logErrorCause(s"Could not renew the browser session while $operation Firestore", Cause.fail(error)) *>
          ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)

  private def validateIdentity(user: SessionUser): ZIO[Requires, RefreshFailure, Unit] =
    val validation = user.identityProvider match
      case IdentityProvider.Google =>
        ZIO
          .fromOption(user.refreshToken)
          .orElseFail(new IllegalStateException("No Google refresh token"))
          .flatMap(GoogleOAuth.accessToken)
          .unit
      case IdentityProvider.Apple =>
        for
          refreshToken <- ZIO
            .fromOption(user.refreshToken)
            .orElseFail(new IllegalStateException("No Apple refresh token"))
          clientId <- ZIO.fromOption(user.providerClientId).orElseFail(new IllegalStateException("No Apple client id"))
          _ <- AppleOAuth.validate(refreshToken, clientId)
        yield ()
      case IdentityProvider.Invitation =>
        for
          subject <- ZIO
            .fromOption(user.providerSubject)
            .orElseFail(new IllegalStateException("No invitation identity"))
          active <- InvitationAccounts.isActive(subject)
          _ <- ZIO.unless(active)(ZIO.fail(new IllegalStateException("Invitation is no longer active")))
        yield ()
    validation.mapError(IdentityValidationFailed(user.identityProvider, _))
