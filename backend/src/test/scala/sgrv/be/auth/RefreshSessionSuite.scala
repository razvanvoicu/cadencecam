package sgrv.be.auth

import java.time.Instant
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import sgrv.be.core.{CapabilityRegistry, PluginStatus, RouteDiscovery}
import zio.{Runtime, Task, UIO, Unsafe, ZEnvironment, ZIO}
import zio.http.{Cookie, Header, Request, Status, URL}

class RefreshSessionSuite extends munit.FunSuite:

  private def run[A](effect: Task[A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
    }

  private def routes(sessionStore: SessionStore, googleOAuth: GoogleOAuth) =
    val registry = CapabilityRegistry.fromEnvironment(ZEnvironment(sessionStore, googleOAuth))
    RouteDiscovery.activate(RefreshSession, RefreshSession.getClass.getName, registry) match
      case PluginStatus.Active(_, _, activeRoutes) => activeRoutes
      case other                                   => fail(s"Expected RefreshSession to activate, got $other")

  private def request(withCookie: Boolean = true): Request =
    val request = Request.post(URL.decode("/refreshSession").toOption.get, zio.http.Body.empty)
    if withCookie then request.addCookie(Cookie.Request(Callback.sessionCookieName, "session-key")) else request

  test("validates the refresh token, extends the stored session, and renews the browser cookie"):
    val refreshedToken = new AtomicReference(Option.empty[String])
    val renewedSession = new AtomicReference(Option.empty[(String, Instant)])
    val user = SessionUser("jane@example.com", "Jane", Some("refresh-token"))
    val store = sessionStore(
      ZIO.some(user),
      (sessionKey, expiresAt) => ZIO.succeed(renewedSession.set(Some(sessionKey -> expiresAt)))
    )
    val oauth = googleOAuth(token => ZIO.succeed(refreshedToken.set(Some(token))).as("access-token"))
    val before = Instant.now()

    val response = run(ZIO.scoped(routes(store, oauth).runZIO(request())))
    val cookie = response.headers.getAll(Header.SetCookie).map(_.value).find(_.name == Callback.sessionCookieName)

    assertEquals(response.status, Status.Ok)
    assertEquals(run(response.body.asString), Me.json(user))
    assertEquals(response.headers.get(RefreshSession.expiresAtHeader), renewedSession.get().map(_._2.toString))
    assertEquals(refreshedToken.get(), Some("refresh-token"))
    assertEquals(renewedSession.get().map(_._1), Some("session-key"))
    assert(renewedSession.get().exists(_._2.isAfter(before.plusSeconds(6 * 24 * 60 * 60))))
    assertEquals(cookie.map(_.content), Some("session-key"))
    assertEquals(cookie.flatMap(_.maxAge), Some(Callback.sessionLifetime))
    assert(cookie.exists(_.isHttpOnly))
    assert(cookie.exists(_.isSecure))
    assertEquals(response.headers.get(Header.CacheControl), Some(Header.CacheControl.NoStore))

  test("returns unauthorized when the cookie, stored session, or refresh token is missing"):
    val googleCalls = new AtomicInteger(0)
    val oauth = googleOAuth(_ => ZIO.succeed(googleCalls.incrementAndGet()).as("access-token"))
    val noSession = sessionStore(ZIO.none, (_, _) => ZIO.unit)
    val noToken = sessionStore(ZIO.some(SessionUser("jane@example.com", "Jane")), (_, _) => ZIO.unit)

    val responses = Seq(
      run(ZIO.scoped(routes(noSession, oauth).runZIO(request(withCookie = false)))),
      run(ZIO.scoped(routes(noSession, oauth).runZIO(request()))),
      run(ZIO.scoped(routes(noToken, oauth).runZIO(request())))
    )

    responses.foreach: response =>
      assertEquals(response.status, Status.Unauthorized)
      assertEquals(response.headers.get(Header.CacheControl), Some(Header.CacheControl.NoStore))
    assertEquals(googleCalls.get(), 0)

  test("returns unauthorized without extending the session when Google rejects the refresh token"):
    val renewals = new AtomicInteger(0)
    val user = SessionUser("jane@example.com", "Jane", Some("revoked-token"))
    val store = sessionStore(ZIO.some(user), (_, _) => ZIO.succeed(renewals.incrementAndGet()).unit)
    val oauth = googleOAuth(_ => ZIO.fail(new RuntimeException("private Google detail")))

    val response = run(ZIO.scoped(routes(store, oauth).runZIO(request())))

    assertEquals(response.status, Status.Unauthorized)
    assertEquals(renewals.get(), 0)
    assertEquals(response.headers.get(Header.CacheControl), Some(Header.CacheControl.NoStore))

  test("returns service unavailable when Firestore cannot read or persist the renewed session"):
    val user = SessionUser("jane@example.com", "Jane", Some("refresh-token"))
    val oauth = googleOAuth(_ => ZIO.succeed("access-token"))
    val readFailure = sessionStore(ZIO.fail(new RuntimeException("private read detail")), (_, _) => ZIO.unit)
    val writeFailure = sessionStore(
      ZIO.some(user),
      (_, _) => ZIO.fail(new RuntimeException("private write detail"))
    )

    Seq(readFailure, writeFailure).foreach: store =>
      val response = run(ZIO.scoped(routes(store, oauth).runZIO(request())))
      assertEquals(response.status, Status.ServiceUnavailable)
      assertEquals(response.headers.get(Header.CacheControl), Some(Header.CacheControl.NoStore))

  private def sessionStore(
      findEffect: Task[Option[SessionUser]],
      renewEffect: (String, Instant) => Task[Unit]
  ): SessionStore =
    new SessionStore:
      override def create(
          sessionKey: String,
          user: SessionUser,
          createdAt: Instant,
          expiresAt: Instant
      ): Task[Unit] = ZIO.unit

      override def find(sessionKey: String, now: Instant): Task[Option[SessionUser]] =
        ZIO.fail(new AssertionError("refreshSession must use the recovery lookup"))

      override def findForRefresh(sessionKey: String): Task[Option[SessionUser]] =
        if sessionKey == "session-key" then findEffect else ZIO.none

      override def renew(sessionKey: String, expiresAt: Instant): Task[Unit] = renewEffect(sessionKey, expiresAt)
      override def invalidate(sessionKey: String): Task[Unit] = ZIO.unit

  private def googleOAuth(accessTokenEffect: String => Task[String]): GoogleOAuth =
    new GoogleOAuth:
      override def authorizationUrl(state: String): UIO[String] = ZIO.succeed("")
      override def authenticate(code: String): Task[GoogleAuthentication] = ZIO.fail(new UnsupportedOperationException)
      override def callbackIsSecure: UIO[Boolean] = ZIO.succeed(true)
      override def accessToken(refreshToken: String): Task[String] = accessTokenEffect(refreshToken)
      override def revoke(refreshToken: String): Task[Unit] = ZIO.unit
