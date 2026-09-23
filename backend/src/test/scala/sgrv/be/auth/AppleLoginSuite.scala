package sgrv.be.auth

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import sgrv.api.MobileSession
import sgrv.be.core.{CapabilityRegistry, LoginEvent, LogoutEvent, PluginStatus, RouteDiscovery, SessionNotifier}
import zio.{Runtime, Task, UIO, Unsafe, ZEnvironment, ZIO}
import zio.http.{Body, Cookie, Header, Request, Status, URL}
import zio.json.*

class AppleLoginSuite extends munit.FunSuite:
  private def run[A](effect: Task[A]): A =
    Unsafe.unsafe { implicit unsafe => Runtime.default.unsafe.run(effect).getOrThrowFiberFailure() }

  test("exchanges a native Apple code for the ordinary bearer session"):
    val created = AtomicReference(Option.empty[(String, SessionUser)])
    val request = Request.post(
      URL.decode("/auth/apple/mobile").toOption.get,
      Body.fromString(
        """{"authorizationCode":"single-use-code","givenName":"Jane","familyName":"Doe"}"""
      )
    )
    val response = run(ZIO.scoped(routes(AppleMobileLogin, created).runZIO(request)))
    val body = run(response.body.asString).fromJson[MobileSession]

    assertEquals(response.status, Status.Ok)
    assertEquals(body.map(_.accessToken), Right("apple-session"))
    assertEquals(created.get().map(_._2.identityProvider), Some(IdentityProvider.Apple))
    assertEquals(created.get().map(_._2.account), Some("apple:apple-subject"))
    assertEquals(created.get().flatMap(_._2.providerClientId), Some("sg.raz.cadencecam"))

  test("accepts Apple's cross-site form callback only with the matching state cookie"):
    val created = AtomicReference(Option.empty[(String, SessionUser)])
    val request = Request
      .post(
        URL.decode("/auth/apple/callback").toOption.get,
        Body.fromString(
          "code=single-use-code&state=matching&user=%7B%22name%22%3A%7B%22firstName%22%3A%22Jane%22%7D%7D"
        )
      )
      .addCookie(Cookie.Request(AppleLogin.stateCookieName, "matching"))
    val response = run(ZIO.scoped(routes(AppleCallback, created).runZIO(request)))
    val cookies = response.headers.getAll(Header.SetCookie).map(_.value)

    assert(response.status.isRedirection)
    assertEquals(created.get().map(_._1), Some("apple-session"))
    assertEquals(cookies.map(_.name).toSet, Set(Callback.sessionCookieName, AppleLogin.stateCookieName))

  test("decodes Apple form_post fields"):
    assertEquals(
      AppleCallback.parseForm("code=a%2Bb&state=with+space"),
      Map("code" -> "a+b", "state" -> "with space")
    )

  private def routes(
      plugin: sgrv.be.core.BackendPlugin,
      created: AtomicReference[Option[(String, SessionUser)]]
  ) =
    val registry = CapabilityRegistry.fromEnvironment(
      ZEnvironment(apple, sessions(created), tokens, notifier)
    )
    RouteDiscovery.activate(plugin, plugin.getClass.getName, registry) match
      case PluginStatus.Active(_, _, activeRoutes) => activeRoutes
      case other                                   => fail(s"Expected ${plugin.id} to activate, got $other")

  private val apple: AppleOAuth = new AppleOAuth:
    override def configured: UIO[Boolean] = ZIO.succeed(true)
    override def authorizationUrl(state: String): Task[String] = ZIO.succeed("https://appleid.apple.com/")
    override def authenticateBrowser(code: String, givenName: Option[String], familyName: Option[String]) =
      authenticateNative(code, givenName, familyName)
    override def authenticateNative(code: String, givenName: Option[String], familyName: Option[String]) =
      assertEquals(code, "single-use-code")
      ZIO.succeed(
        AppleAuthentication(
          SessionUser(
            "relay@privaterelay.appleid.com",
            givenName.getOrElse("Apple user"),
            refreshToken = Some("apple-refresh"),
            identityProvider = IdentityProvider.Apple,
            providerSubject = Some("apple-subject"),
            accountId = Some("apple:apple-subject"),
            providerClientId = Some("sg.raz.cadencecam")
          )
        )
      )
    override def validate(refreshToken: String, clientId: String): Task[Unit] = ZIO.unit
    override def revoke(token: String, clientId: String): Task[Unit] = ZIO.unit
    override def callbackIsSecure: UIO[Boolean] = ZIO.succeed(true)

  private def sessions(created: AtomicReference[Option[(String, SessionUser)]]): SessionStore = new SessionStore:
    override def create(key: String, user: SessionUser, createdAt: Instant, expiresAt: Instant): Task[Unit] =
      ZIO.succeed(created.set(Some(key -> user)))
    override def find(sessionKey: String, now: Instant) = ZIO.none
    override def findForRefresh(sessionKey: String) = ZIO.none
    override def renew(sessionKey: String, expiresAt: Instant) = ZIO.unit
    override def invalidate(sessionKey: String) = ZIO.unit
    override def invalidateAll(accountId: String) = ZIO.unit

  private val tokens: TokenGenerator = new TokenGenerator:
    override def generate(bytes: Int): UIO[String] = ZIO.succeed("apple-session")

  private val notifier: SessionNotifier = new SessionNotifier:
    override def loginSucceeded(event: LoginEvent): UIO[Unit] = ZIO.unit
    override def loggedOut(event: LogoutEvent): UIO[Unit] = ZIO.unit
