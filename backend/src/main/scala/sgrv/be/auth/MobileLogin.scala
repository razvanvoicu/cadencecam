package sgrv.be.auth

import sgrv.api.{CurrentUser, MobileAuthentication, MobileSession}
import sgrv.be.BackendCapabilities
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, LoginEvent, RequestContext, SessionNotifier}
import zio.{Cause, Clock, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

/** Exchanges a native Google ID token for the same opaque application session used by the browser.
  *
  * Google Sign-In on the handset authenticates the person. The ID token is sent once, verified for this backend's web
  * client id, and discarded. The returned token is random application state stored in Firestore; every existing route
  * can therefore authorize native and browser clients identically without accepting a Google credential on each call.
  */
object MobileLogin extends BackendPlugin:
  type Requires = GoogleOAuth & SessionStore & TokenGenerator & SessionNotifier

  override val id = "auth-mobile"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.googleOAuth) ++
      CapabilitySet.one(BackendCapabilities.sessionStore) ++
      CapabilitySet.one(BackendCapabilities.tokenGenerator) ++
      CapabilitySet.one(BackendCapabilities.sessionNotifier)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.POST / "auth" / "mobile" -> handler((request: Request) => apply(request)))

  private def apply(request: Request): ZIO[Requires, Nothing, Response] =
    val login = for
      body <- request.body.asString
      submitted <- ZIO.fromEither(body.fromJson[MobileAuthentication]).mapError(new IllegalArgumentException(_))
      idToken <- ZIO
        .fromOption(Option(submitted.idToken).map(_.trim).filter(token => token.nonEmpty && token.length <= 16384))
        .orElseFail(new IllegalArgumentException("The Google ID token is missing or too large"))
      authentication <- GoogleOAuth.authenticateIdToken(idToken)
      now <- Clock.instant
      expiry = now.plus(Callback.sessionLifetime)
      sessionKey <- TokenGenerator.generate(32)
      _ <- SessionStore.create(sessionKey, authentication.user, now, expiry)
      _ <- SessionNotifier.loginSucceeded(LoginEvent(sessionKey, authentication.user, now))
      session = MobileSession(
        sessionKey,
        expiry.toEpochMilli,
        CurrentUser(authentication.user.email, authentication.user.name)
      )
    yield Response.json(session.toJson).addHeader(Header.CacheControl.NoStore)

    login.catchAll: error =>
      ZIO.logWarningCause("Native Google login failed", Cause.fail(error)) *>
        ZIO.succeed(
          Response
            .text("Could not authenticate with Google.")
            .status(Status.Unauthorized)
            .addHeader(Header.CacheControl.NoStore)
        )
