package sgrv.be.auth

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.net.URLDecoder
import sgrv.api.{AppleMobileAuthentication, CurrentUser, MobileSession}
import sgrv.be.BackendCapabilities
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, LoginEvent, RequestContext, SessionNotifier}
import zio.{durationInt, Cause, Clock, Duration, IO, ZIO}
import zio.http.{Cookie, Header, Method, Path, Request, Response, Routes, Status, URL, handler}
import zio.json.*

/** Browser entry point for Apple's OAuth authorization-code flow. */
object AppleLogin extends BackendPlugin:
  type Requires = AppleOAuth & TokenGenerator

  override val id = "auth-apple-login"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.appleOAuth) ++ CapabilitySet.one(BackendCapabilities.tokenGenerator)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.GET / "auth" / "apple" / "login" -> handler((_: Request) => apply))

  private[auth] val stateCookieName = "apple_auth_state"
  private[auth] val cookiePath = Path.root / "auth" / "apple"

  private def apply: ZIO[Requires, Nothing, Response] =
    val redirect = for
      state <- TokenGenerator.generate(32)
      target <- AppleOAuth.authorizationUrl(state)
      secure <- AppleOAuth.callbackIsSecure
      _ <- ZIO.unless(secure)(ZIO.fail(new IllegalStateException("Apple browser login requires HTTPS")))
      url <- ZIO.fromEither(URL.decode(target))
    yield Response.redirect(url).addCookie(stateCookie(state))

    redirect.catchAll: error =>
      ZIO.logWarningCause("Apple login could not start", Cause.fail(error)) *>
        ZIO.succeed(Response.text("Sign in with Apple is currently unavailable.").status(Status.ServiceUnavailable))

  private def stateCookie(state: String): Cookie.Response = Cookie.Response(
    name = stateCookieName,
    content = state,
    path = Some(cookiePath),
    isSecure = true,
    isHttpOnly = true,
    maxAge = Some(10.minutes),
    // Apple's form_post callback is a cross-site POST. Lax cookies are deliberately withheld from it.
    sameSite = Some(Cookie.SameSite.None)
  )

  private[auth] def clearedStateCookie: Cookie.Response = Cookie.Response(
    name = stateCookieName,
    content = "",
    path = Some(cookiePath),
    isSecure = true,
    isHttpOnly = true,
    maxAge = Some(Duration.Zero),
    sameSite = Some(Cookie.SameSite.None)
  )

/** Accepts Apple's form_post callback, verifies state, and creates the ordinary browser session. */
object AppleCallback extends BackendPlugin:
  type Requires = AppleOAuth & SessionStore & TokenGenerator & SessionNotifier

  override val id = "auth-apple-callback"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.appleOAuth) ++
      CapabilitySet.one(BackendCapabilities.sessionStore) ++
      CapabilitySet.one(BackendCapabilities.tokenGenerator) ++
      CapabilitySet.one(BackendCapabilities.sessionNotifier)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.POST / "auth" / "apple" / "callback" -> handler((request: Request) => apply(request)))

  private def apply(request: Request): ZIO[Requires, Nothing, Response] =
    val login = for
      body <- request.body.asString
      form <- ZIO.attempt(parseForm(body))
      _ <- checkState(request, form.getOrElse("state", ""))
      code <- ZIO.fromOption(form.get("code").filter(_.nonEmpty)).orElseFail(
        new IllegalArgumentException(form.get("error").fold("Apple returned no authorization code")(e => s"Apple reported: $e"))
      )
      (givenName, familyName) = names(form.get("user"))
      authentication <- AppleOAuth.authenticateBrowser(code, givenName, familyName)
      now <- Clock.instant
      expiry = now.plus(Callback.sessionLifetime)
      sessionKey <- TokenGenerator.generate(32)
      _ <- SessionStore.create(sessionKey, authentication.user, now, expiry)
      _ <- SessionNotifier.loginSucceeded(LoginEvent(sessionKey, authentication.user, now))
      secure <- AppleOAuth.callbackIsSecure
    yield Response
      .redirect(URL.root)
      .addCookie(Callback.sessionCookie(sessionKey, secure))
      .addCookie(AppleLogin.clearedStateCookie)

    login.catchAll: error =>
      ZIO.logWarningCause("Apple browser login failed", Cause.fail(error)) *>
        ZIO.succeed(
          Response
            .text("Sign in with Apple failed. Return to the home page and try again.")
            .status(Status.Unauthorized)
            .addCookie(AppleLogin.clearedStateCookie)
        )

  private def checkState(request: Request, provided: String): IO[IllegalArgumentException, Unit] =
    val expected = request.cookie(AppleLogin.stateCookieName).map(_.content).getOrElse("")
    if provided.nonEmpty && expected.nonEmpty &&
      MessageDigest.isEqual(provided.getBytes(UTF_8), expected.getBytes(UTF_8))
    then ZIO.unit
    else ZIO.fail(new IllegalArgumentException("The Apple state parameter does not match the login cookie"))

  private[auth] def parseForm(body: String): Map[String, String] =
    Option(body).getOrElse("").split("&").iterator.filter(_.nonEmpty).map: pair =>
      val parts = pair.split("=", 2)
      decode(parts(0)) -> decode(if parts.length == 2 then parts(1) else "")
    .toMap

  private def decode(value: String): String = URLDecoder.decode(value, UTF_8)

  private def names(user: Option[String]): (Option[String], Option[String]) =
    val fields = user.flatMap(_.fromJson[AppleCallbackUser].toOption).flatMap(_.name)
    fields.flatMap(_.firstName) -> fields.flatMap(_.lastName)

  @jsonNoExtraFields
  private final case class AppleCallbackName(firstName: Option[String] = None, lastName: Option[String] = None)
  private object AppleCallbackName:
    given JsonDecoder[AppleCallbackName] = DeriveJsonDecoder.gen[AppleCallbackName]
  @jsonNoExtraFields
  private final case class AppleCallbackUser(name: Option[AppleCallbackName] = None, email: Option[String] = None)
  private object AppleCallbackUser:
    given JsonDecoder[AppleCallbackUser] = DeriveJsonDecoder.gen[AppleCallbackUser]

/** Exchanges the native iOS authorization code for an ordinary CadenceCam bearer session. */
object AppleMobileLogin extends BackendPlugin:
  type Requires = AppleOAuth & SessionStore & TokenGenerator & SessionNotifier

  override val id = "auth-apple-mobile"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.appleOAuth) ++
      CapabilitySet.one(BackendCapabilities.sessionStore) ++
      CapabilitySet.one(BackendCapabilities.tokenGenerator) ++
      CapabilitySet.one(BackendCapabilities.sessionNotifier)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.POST / "auth" / "apple" / "mobile" -> handler((request: Request) => apply(request)))

  private def apply(request: Request): ZIO[Requires, Nothing, Response] =
    val login = for
      body <- request.body.asString
      submitted <- ZIO.fromEither(body.fromJson[AppleMobileAuthentication]).mapError(new IllegalArgumentException(_))
      authentication <- AppleOAuth.authenticateNative(
        submitted.authorizationCode,
        submitted.givenName,
        submitted.familyName
      )
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
      ZIO.logWarningCause("Native Apple login failed", Cause.fail(error)) *>
        ZIO.succeed(
          Response
            .text("Could not authenticate with Apple.")
            .status(Status.Unauthorized)
            .addHeader(Header.CacheControl.NoStore)
        )
