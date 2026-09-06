package sgrv.be.auth

import com.google.api.client.http.{HttpHeaders, HttpResponseException}
import java.time.Instant
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import sgrv.be.core.{CapabilityRegistry, PluginStatus, RouteDiscovery}
import zio.{Duration, Runtime, Task, UIO, Unsafe, ZEnvironment, ZIO}
import zio.http.{Cookie, Header, Request, Status, URL}

class LogoutSuite extends munit.FunSuite:

  private def run[A](effect: Task[A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
    }

  private def routes(sessionStore: SessionStore, googleOAuth: GoogleOAuth) =
    val registry = CapabilityRegistry.fromEnvironment(ZEnvironment(sessionStore, googleOAuth))
    RouteDiscovery.activate(Logout, Logout.getClass.getName, registry) match
      case PluginStatus.Active(_, _, activeRoutes) => activeRoutes
      case other                                   => fail(s"Expected Logout to activate, got $other")

  private def request = Request
    .post(URL.decode("/logout").toOption.get, zio.http.Body.empty)
    .addCookie(Cookie.Request(Callback.sessionCookieName, "session-key"))

  test("revokes Google authorization, deletes the session, and expires authentication cookies"):
    val revokedToken = new AtomicReference(Option.empty[String])
    val invalidatedSession = new AtomicReference(Option.empty[String])
    val user = SessionUser("jane@example.com", "Jane", Some("refresh-token"))
    val store = sessionStore(user, sessionKey => ZIO.succeed(invalidatedSession.set(Some(sessionKey))))
    val oauth = googleOAuth(token => ZIO.succeed(revokedToken.set(Some(token))))

    val response = run(ZIO.scoped(routes(store, oauth).runZIO(request)))
    val cookies = response.headers.getAll(Header.SetCookie).map(_.value)

    assertEquals(response.status, Status.NoContent)
    assertEquals(revokedToken.get(), Some("refresh-token"))
    assertEquals(invalidatedSession.get(), Some("session-key"))
    assertEquals(cookies.map(_.name).toSet, Set(Callback.sessionCookieName, Login.stateCookieName))
    cookies.foreach: cookie =>
      assertEquals(cookie.content, "")
      assertEquals(cookie.maxAge, Some(Duration.Zero))
      assert(cookie.isHttpOnly)
    assertEquals(response.headers.get(Header.CacheControl), Some(Header.CacheControl.NoStore))

  test("does not delete the retryable session when Google revocation fails"):
    val invalidations = new AtomicInteger(0)
    val user = SessionUser("jane@example.com", "Jane", Some("refresh-token"))
    val store = sessionStore(user, _ => ZIO.succeed(invalidations.incrementAndGet()).unit)
    val oauth = googleOAuth(_ => ZIO.fail(new RuntimeException("private upstream detail")))

    val response = run(ZIO.scoped(routes(store, oauth).runZIO(request)))

    assertEquals(response.status, Status.BadGateway)
    assertEquals(run(response.body.asString), "Could not revoke Google authorization. Try again.")
    assertEquals(invalidations.get(), 0)
    assertEquals(response.headers.getAll(Header.SetCookie).size, 0)

  test("invalidates a session without calling Google when no refresh token was stored"):
    val revocations = new AtomicInteger(0)
    val invalidations = new AtomicInteger(0)
    val store = sessionStore(
      SessionUser("jane@example.com", "Jane"),
      _ => ZIO.succeed(invalidations.incrementAndGet()).unit
    )
    val oauth = googleOAuth(_ => ZIO.succeed(revocations.incrementAndGet()).unit)

    val response = run(ZIO.scoped(routes(store, oauth).runZIO(request)))

    assertEquals(response.status, Status.NoContent)
    assertEquals(revocations.get(), 0)
    assertEquals(invalidations.get(), 1)

  test("revokes the fallback access token when Google issued no refresh token"):
    val revokedToken = new AtomicReference(Option.empty[String])
    val store = sessionStore(
      SessionUser("jane@example.com", "Jane", accessTokenForRevocation = Some("access-token")),
      _ => ZIO.unit
    )
    val oauth = googleOAuth(token => ZIO.succeed(revokedToken.set(Some(token))))

    val response = run(ZIO.scoped(routes(store, oauth).runZIO(request)))

    assertEquals(response.status, Status.NoContent)
    assertEquals(revokedToken.get(), Some("access-token"))

  test("keeps the browser cookie retryable when Firestore invalidation fails"):
    val revocations = new AtomicInteger(0)
    val user = SessionUser("jane@example.com", "Jane", Some("refresh-token"))
    val store = sessionStore(user, _ => ZIO.fail(new RuntimeException("private Firestore detail")))
    val oauth = googleOAuth(_ => ZIO.succeed(revocations.incrementAndGet()).unit)

    val response = run(ZIO.scoped(routes(store, oauth).runZIO(request)))

    assertEquals(response.status, Status.ServiceUnavailable)
    assertEquals(run(response.body.asString), "Could not invalidate the browser session. Try again.")
    assertEquals(revocations.get(), 1)
    assertEquals(response.headers.getAll(Header.SetCookie).size, 0)

  test("treats an already expired or revoked Google token as an idempotent success"):
    val alreadyRevoked = new HttpResponseException.Builder(400, "Bad Request", new HttpHeaders())
      .setContent("""{"error":"invalid_token"}""")
      .build()
    val malformed = new HttpResponseException.Builder(400, "Bad Request", new HttpHeaders())
      .setContent("""{"error":"invalid_request"}""")
      .build()

    assert(GoogleOAuth.isAlreadyRevoked(alreadyRevoked))
    assert(!GoogleOAuth.isAlreadyRevoked(malformed))

  private def sessionStore(user: SessionUser, invalidateEffect: String => Task[Unit]): SessionStore =
    new SessionStore:
      override def create(
          sessionKey: String,
          user: SessionUser,
          createdAt: Instant,
          expiresAt: Instant
      ): Task[Unit] = ZIO.unit

      override def find(sessionKey: String, now: Instant): Task[Option[SessionUser]] =
        ZIO.succeed(Option.when(sessionKey == "session-key")(user))

      override def findForRefresh(sessionKey: String): Task[Option[SessionUser]] = ZIO.none
      override def renew(sessionKey: String, expiresAt: Instant): Task[Unit] = ZIO.unit
      override def invalidate(sessionKey: String): Task[Unit] = invalidateEffect(sessionKey)

  private def googleOAuth(revokeEffect: String => Task[Unit]): GoogleOAuth =
    new GoogleOAuth:
      override def authorizationUrl(state: String): UIO[String] = ZIO.succeed("")
      override def authenticate(code: String): Task[GoogleAuthentication] = ZIO.fail(new UnsupportedOperationException)
      override def callbackIsSecure: UIO[Boolean] = ZIO.succeed(true)
      override def accessToken(refreshToken: String): Task[String] = ZIO.fail(new UnsupportedOperationException)
      override def revoke(refreshToken: String): Task[Unit] = revokeEffect(refreshToken)
