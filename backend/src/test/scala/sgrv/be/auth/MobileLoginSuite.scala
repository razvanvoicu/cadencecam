package sgrv.be.auth

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import sgrv.api.MobileSession
import sgrv.be.core.{CapabilityRegistry, LoginEvent, LogoutEvent, PluginStatus, RouteDiscovery, SessionNotifier}
import zio.{Runtime, Task, UIO, Unsafe, ZEnvironment, ZIO}
import zio.http.{Body, Header, Request, Status, URL}
import zio.json.*

class MobileLoginSuite extends munit.FunSuite:

  private def run[A](effect: Task[A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
    }

  test("exchanges a verified Google ID token for an opaque application session"):
    val created = new AtomicReference(Option.empty[(String, SessionUser, Instant, Instant)])
    val loggedIn = new AtomicReference(Option.empty[LoginEvent])
    val user = SessionUser("jane@example.com", "Jane")
    val request = Request.post(
      URL.decode("/auth/mobile").toOption.get,
      Body.fromString("""{"idToken":"google-id-token"}""")
    )

    val response = run(ZIO.scoped(routes(user, created, loggedIn).runZIO(request)))
    val body = run(response.body.asString).fromJson[MobileSession]

    assertEquals(response.status, Status.Ok)
    assertEquals(response.headers.get(Header.CacheControl), Some(Header.CacheControl.NoStore))
    assertEquals(body.map(_.accessToken), Right("native-session-key"))
    assertEquals(body.map(_.user.email), Right("jane@example.com"))
    assertEquals(body.map(_.user.name), Right("Jane"))
    assertEquals(created.get().map(_._1), Some("native-session-key"))
    assertEquals(created.get().map(_._2), Some(user))
    assertEquals(loggedIn.get().map(_.sessionKey), Some("native-session-key"))
    assertEquals(loggedIn.get().map(_.user), Some(user))
    assert(created.get().exists { case (_, _, at, expiry) => expiry == at.plus(Callback.sessionLifetime) })

  test("does not create a session when Google refuses the native token"):
    val created = new AtomicReference(Option.empty[(String, SessionUser, Instant, Instant)])
    val loggedIn = new AtomicReference(Option.empty[LoginEvent])
    val request = Request.post(
      URL.decode("/auth/mobile").toOption.get,
      Body.fromString("""{"idToken":"wrong"}""")
    )

    val response = run(ZIO.scoped(routes(SessionUser("jane@example.com", "Jane"), created, loggedIn, reject = true)
      .runZIO(request)))

    assertEquals(response.status, Status.Unauthorized)
    assertEquals(created.get(), None)
    assertEquals(loggedIn.get(), None)
    assertEquals(run(response.body.asString), "Could not authenticate with Google.")

  private def routes(
      user: SessionUser,
      created: AtomicReference[Option[(String, SessionUser, Instant, Instant)]],
      loggedIn: AtomicReference[Option[LoginEvent]],
      reject: Boolean = false
  ) =
    val oauth = new GoogleOAuth:
      override def authorizationUrl(state: String): UIO[String] = ZIO.succeed("")
      override def authenticate(code: String): Task[GoogleAuthentication] = ZIO.fail(new UnsupportedOperationException)
      override def authenticateIdToken(idToken: String): Task[GoogleAuthentication] =
        if reject then ZIO.fail(new IllegalArgumentException("private verification detail"))
        else
          assertEquals(idToken, "google-id-token")
          ZIO.succeed(GoogleAuthentication(user))
      override def callbackIsSecure: UIO[Boolean] = ZIO.succeed(true)
      override def accessToken(refreshToken: String): Task[String] = ZIO.fail(new UnsupportedOperationException)
      override def revoke(refreshToken: String): Task[Unit] = ZIO.unit

    val store = new SessionStore:
      override def create(
          sessionKey: String,
          storedUser: SessionUser,
          createdAt: Instant,
          expiresAt: Instant
      ): Task[Unit] = ZIO.succeed(created.set(Some((sessionKey, storedUser, createdAt, expiresAt))))
      override def find(sessionKey: String, now: Instant): Task[Option[SessionUser]] = ZIO.none
      override def findForRefresh(sessionKey: String): Task[Option[SessionUser]] = ZIO.none
      override def renew(sessionKey: String, expiresAt: Instant): Task[Unit] = ZIO.unit
      override def invalidate(sessionKey: String): Task[Unit] = ZIO.unit

    val tokens = new TokenGenerator:
      override def generate(bytes: Int): UIO[String] =
        assertEquals(bytes, 32)
        ZIO.succeed("native-session-key")

    val notifier = new SessionNotifier:
      override def loginSucceeded(event: LoginEvent): UIO[Unit] = ZIO.succeed(loggedIn.set(Some(event)))
      override def loggedOut(event: LogoutEvent): UIO[Unit] = ZIO.unit

    val registry = CapabilityRegistry.fromEnvironment(ZEnvironment(oauth, store, tokens, notifier))
    RouteDiscovery.activate(MobileLogin, MobileLogin.getClass.getName, registry) match
      case PluginStatus.Active(_, _, activeRoutes) => activeRoutes
      case other                                   => fail(s"Expected MobileLogin to activate, got $other")
