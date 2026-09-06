package sgrv.be.auth

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import sgrv.be.BackendCapabilities
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{durationInt, Clock, Duration, IO, ZIO}
import zio.http.{Cookie, Method, Path, Request, Response, Routes, Status, URL, handler}

/** Completes the Google login and creates an opaque browser session in Firestore. */
object Callback extends BackendPlugin:
  type Requires = GoogleOAuth & SessionStore & TokenGenerator

  override val id = "auth-callback"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.googleOAuth) ++
      CapabilitySet.one(BackendCapabilities.sessionStore) ++
      CapabilitySet.one(BackendCapabilities.tokenGenerator)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.GET / "auth" / "callback" -> handler((request: Request) => apply(request)))

  private[auth] val sessionCookieName = "session"
  private[auth] val sessionLifetime = 7.days

  private def apply(request: Request): ZIO[Requires, Nothing, Response] =
    val login =
      for
        _ <- checkState(request)
        code <- ZIO
          .fromOption(request.queryParam("code").filter(_.nonEmpty))
          .orElseFail(
            new IllegalArgumentException(
              request
                .queryParam("error")
                .fold("The callback carries no authorization code")(e => s"Google reported: $e")
            )
          )
        authentication <- GoogleOAuth.authenticate(code)
        now <- Clock.instant
        expiry = now.plus(sessionLifetime)
        sessionKey <- TokenGenerator.generate(32)
        _ <- SessionStore.create(sessionKey, authentication.user, now, expiry)
        _ <- ZIO.logInfo(s"Created browser session for ${authentication.user.email}")
        secure <- GoogleOAuth.callbackIsSecure
        cookie = sessionCookie(sessionKey, secure)
      yield Response.redirect(URL.root).addCookie(cookie).addCookie(clearedStateCookie(secure))
    login.catchAll: error =>
      ZIO.logWarning(s"Google login failed: ${error.getMessage}") *>
        ZIO.succeed(Response.text("Login failed. Return to the home page and try again.").status(Status.Unauthorized))

  /** Checks that the state parameter in the request matches the login cookie.
    *
    * @param request
    *   An HTTP request
    * @return
    *   Unit
    */
  private def checkState(request: Request): IO[IllegalArgumentException, Unit] =
    val provided = request.queryParam("state").getOrElse("")
    val expected = request.cookie(Login.stateCookieName).map(_.content).getOrElse("")
    if provided.nonEmpty && expected.nonEmpty &&
      MessageDigest.isEqual(provided.getBytes(UTF_8), expected.getBytes(UTF_8))
    then ZIO.unit
    else ZIO.fail(new IllegalArgumentException("The state parameter does not match the login cookie"))

  /** Creates an HTTP cookie that stores the session token.
    *
    * @param token
    *   The session token
    * @param secure
    *   Whether the cookie should be marked as secure
    * @return
    *   An HTTP cookie
    */
  private[auth] def sessionCookie(token: String, secure: Boolean): Cookie.Response =
    Cookie.Response(
      name = sessionCookieName,
      content = token,
      path = Option(Path.root),
      isSecure = secure,
      isHttpOnly = true,
      maxAge = Option(sessionLifetime),
      sameSite = Option(Cookie.SameSite.Lax)
    )

  /** Creates an HTTP cookie that clears the login state cookie.
    *
    * @param secure
    *   Whether the cookie should be marked as secure
    * @return
    *   An HTTP cookie
    */
  private[auth] def clearedStateCookie(secure: Boolean): Cookie.Response =
    Cookie.Response(
      name = Login.stateCookieName,
      content = "",
      path = Option(Login.cookiePath),
      isSecure = secure,
      isHttpOnly = true,
      maxAge = Option(Duration.Zero)
    )

  private[auth] def clearedSessionCookie(secure: Boolean): Cookie.Response =
    Cookie.Response(
      name = sessionCookieName,
      content = "",
      path = Option(Path.root),
      isSecure = secure,
      isHttpOnly = true,
      maxAge = Option(Duration.Zero),
      sameSite = Option(Cookie.SameSite.Lax)
    )
