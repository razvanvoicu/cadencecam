package sgrv.be.auth

import sgrv.api.{CurrentUser, InvitationAuthentication, MobileSession}
import sgrv.be.BackendCapabilities
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, LoginEvent, RequestContext, SessionNotifier}
import zio.{durationInt, Cause, Clock, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

/** Exchanges reusable invitation credentials for an ordinary CadenceCam session.
  *
  * The same endpoint serves browsers and native apps. Browsers retain the HttpOnly cookie; native clients retain the
  * opaque token in the JSON response. From then on neither client receives special treatment from the account,
  * workout, Counter or dashboard code.
  */
object InvitationLogin extends BackendPlugin:
  type Requires = InvitationAccounts & SessionStore & TokenGenerator & SessionNotifier & GoogleOAuth

  override val id = "auth-invitation"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.invitationAccounts) ++
      CapabilitySet.one(BackendCapabilities.sessionStore) ++
      CapabilitySet.one(BackendCapabilities.tokenGenerator) ++
      CapabilitySet.one(BackendCapabilities.sessionNotifier) ++
      CapabilitySet.one(BackendCapabilities.googleOAuth)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.POST / "auth" / "invitation" -> handler((request: Request) => apply(request)))

  private def apply(request: Request): ZIO[Requires, Nothing, Response] =
    val login = for
      body <- request.body.asString
      submitted <- ZIO.fromEither(body.fromJson[InvitationAuthentication]).mapError(new IllegalArgumentException(_))
      now <- Clock.instant
      user <- InvitationAccounts.authenticate(submitted.username, submitted.accessCode, now)
      expiry = now.plus(Callback.sessionLifetime)
      sessionKey <- TokenGenerator.generate(32)
      _ <- SessionStore.create(sessionKey, user, now, expiry)
      _ <- SessionNotifier.loginSucceeded(LoginEvent(sessionKey, user, now))
      secure <- GoogleOAuth.callbackIsSecure
      session = MobileSession(sessionKey, expiry.toEpochMilli, CurrentUser(user.email, user.name))
    yield Response
      .json(session.toJson)
      .addHeader(Header.CacheControl.NoStore)
      .addCookie(Callback.sessionCookie(sessionKey, secure))

    login.catchAll: error =>
      // Deliberately generic: neither an unknown username nor a revoked/deleted invitation is disclosed.
      ZIO.logWarningCause("Invitation login failed", Cause.fail(error)) *>
        ZIO.sleep(250.millis) *>
        ZIO.succeed(
          Response
            .text("The invitation username or access code is not valid.")
            .status(Status.Unauthorized)
            .addHeader(Header.CacheControl.NoStore)
        )
