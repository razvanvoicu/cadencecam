package sgrv.be.auth

import sgrv.be.BackendCapabilities
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{durationInt, ZIO}
import zio.http.{Cookie, Method, Path, Request, Response, Routes, Status, URL, handler}

/** Starts the Google OAuth 2.0 authorization-code flow with a CSRF-protecting `state` cookie. */
object Login extends BackendPlugin:
  type Requires = GoogleOAuth & TokenGenerator

  override val id = "auth-login"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.googleOAuth) ++ CapabilitySet.one(BackendCapabilities.tokenGenerator)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.GET / "auth" / "login" -> handler((_: Request) => apply))

  private[auth] val stateCookieName = "auth_state"
  private[auth] val cookiePath = Path.root / "auth"

  private def apply: ZIO[Requires, Nothing, Response] =
    val redirect =
      for
        state <- TokenGenerator.generate(32)
        target <- GoogleOAuth.authorizationUrl(state)
        callbackSecure <- GoogleOAuth.callbackIsSecure
        url <- ZIO.fromEither(URL.decode(target))
      yield Response
        .redirect(url)
        .addCookie(stateCookie(state, secure = callbackSecure))
    redirect.catchAll: error =>
      ZIO.logWarning(s"Google login could not start: ${error.getMessage}") *>
        ZIO.succeed(Response.text("Login is currently unavailable.").status(Status.ServiceUnavailable))

  /** Generate the cookie that links the Google Auth login call to the ensuing callback. Upon receiving this cookie the
    * backend can be sure that it responds to a login attempt from the same session.
    *
    * @param state
    *   Generated random key to be verified by the callback
    * @param secure
    *   Usually 'true'
    * @return
    */
  private def stateCookie(state: String, secure: Boolean): Cookie.Response =
    Cookie.Response(
      name = stateCookieName,
      content = state,
      path = Some(cookiePath),
      isSecure = secure,
      isHttpOnly = true,
      maxAge = Some(10.minutes),
      sameSite = Some(Cookie.SameSite.Lax)
    )
