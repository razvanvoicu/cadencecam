package sgrv.be.auth

import com.google.api.client.http.{HttpHeaders, HttpResponseException}
import java.time.Instant
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import sgrv.be.core.{CapabilityRegistry, LoginEvent, LogoutEvent, PluginStatus, RouteDiscovery, SessionNotifier}
import sgrv.be.sessions.WorkoutSessions
import zio.{Duration, Runtime, Task, UIO, Unsafe, ZEnvironment, ZIO}
import zio.http.{Cookie, Header, Request, Status, URL}

class LogoutSuite extends munit.FunSuite:

  private def run[A](effect: Task[A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
    }

  /** Records the logout events the route raises, so a test can assert both that it fires and that it does not. */
  private def recordingNotifier(sink: AtomicReference[List[LogoutEvent]]): SessionNotifier =
    new SessionNotifier:
      override def loginSucceeded(event: LoginEvent): UIO[Unit] = ZIO.unit
      override def loggedOut(event: LogoutEvent): UIO[Unit] = ZIO.succeed { val _ = sink.updateAndGet(event :: _) }

  private def routes(
      sessionStore: SessionStore,
      googleOAuth: GoogleOAuth,
      notifier: SessionNotifier = SessionNotifier.none,
      workouts: WorkoutSessions = workoutSessions(_ => ZIO.unit)
  ) =
    val registry = CapabilityRegistry.fromEnvironment(ZEnvironment(sessionStore, googleOAuth, notifier, workouts))
    RouteDiscovery.activate(Logout, Logout.getClass.getName, registry) match
      case PluginStatus.Active(_, _, activeRoutes) => activeRoutes
      case other                                   => fail(s"Expected Logout to activate, got $other")

  private def request = Request
    .post(URL.decode("/logout").toOption.get, zio.http.Body.empty)
    .addCookie(Cookie.Request(Callback.sessionCookieName, "session-key"))

  test("closes the workout, revokes Google, invalidates every device, and expires authentication cookies"):
    val revokedToken = new AtomicReference(Option.empty[String])
    val invalidatedAccount = new AtomicReference(Option.empty[String])
    val closedAccount = new AtomicReference(Option.empty[String])
    val operations = new AtomicReference(List.empty[String])
    def mark(name: String): Unit = { val _ = operations.updateAndGet(name :: _); () }
    val user = SessionUser("jane@example.com", "Jane", Some("refresh-token"))
    val store = sessionStore(user, email => ZIO.succeed { invalidatedAccount.set(Some(email)); mark("invalidate") })
    val oauth = googleOAuth(token => ZIO.succeed { revokedToken.set(Some(token)); mark("revoke") })
    val workouts = workoutSessions(email => ZIO.succeed { closedAccount.set(Some(email)); mark("close") })

    val response = run(ZIO.scoped(routes(store, oauth, workouts = workouts).runZIO(request)))
    val cookies = response.headers.getAll(Header.SetCookie).map(_.value)

    assertEquals(response.status, Status.NoContent)
    assertEquals(closedAccount.get(), Some("jane@example.com"))
    assertEquals(revokedToken.get(), Some("refresh-token"))
    assertEquals(invalidatedAccount.get(), Some("jane@example.com"))
    assertEquals(operations.get().reverse, List("close", "revoke", "invalidate"))
    assertEquals(cookies.map(_.name).toSet, Set(Callback.sessionCookieName, Login.stateCookieName))
    cookies.foreach: cookie =>
      assertEquals(cookie.content, "")
      assertEquals(cookie.maxAge, Some(Duration.Zero))
      assert(cookie.isHttpOnly)
    assertEquals(response.headers.get(Header.CacheControl), Some(Header.CacheControl.NoStore))

  test("closes the workout but keeps credentials retryable when Google revocation fails"):
    val invalidations = new AtomicInteger(0)
    val closures = new AtomicInteger(0)
    val user = SessionUser("jane@example.com", "Jane", Some("refresh-token"))
    val store = sessionStore(user, _ => ZIO.succeed(invalidations.incrementAndGet()).unit)
    val oauth = googleOAuth(_ => ZIO.fail(new RuntimeException("private upstream detail")))

    val response = run(ZIO.scoped(
      routes(store, oauth, workouts = workoutSessions(_ => ZIO.succeed(closures.incrementAndGet()).unit)).runZIO(request)
    ))

    assertEquals(response.status, Status.BadGateway)
    assertEquals(run(response.body.asString), "Could not revoke Google authorization. Try again.")
    assertEquals(closures.get(), 1)
    assertEquals(invalidations.get(), 0)
    assertEquals(response.headers.getAll(Header.SetCookie).size, 0)

  test("invalidates every device without calling Google when no revocation token was stored"):
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

  test("keeps the browser cookie retryable when account-wide invalidation fails"):
    val revocations = new AtomicInteger(0)
    val user = SessionUser("jane@example.com", "Jane", Some("refresh-token"))
    val store = sessionStore(user, _ => ZIO.fail(new RuntimeException("private Firestore detail")))
    val oauth = googleOAuth(_ => ZIO.succeed(revocations.incrementAndGet()).unit)

    val response = run(ZIO.scoped(routes(store, oauth).runZIO(request)))

    assertEquals(response.status, Status.ServiceUnavailable)
    assertEquals(run(response.body.asString), "Could not invalidate every signed-in device. Try again.")
    assertEquals(revocations.get(), 1)
    assertEquals(response.headers.getAll(Header.SetCookie).size, 0)

  test("does not revoke or invalidate credentials when the active workout cannot close"):
    val revocations = new AtomicInteger(0)
    val invalidations = new AtomicInteger(0)
    val user = SessionUser("jane@example.com", "Jane", Some("refresh-token"))
    val store = sessionStore(user, _ => ZIO.succeed(invalidations.incrementAndGet()).unit)
    val oauth = googleOAuth(_ => ZIO.succeed(revocations.incrementAndGet()).unit)
    val workouts = workoutSessions(_ => ZIO.fail(new RuntimeException("private workout detail")))

    val response = run(ZIO.scoped(routes(store, oauth, workouts = workouts).runZIO(request)))

    assertEquals(response.status, Status.ServiceUnavailable)
    assertEquals(run(response.body.asString), "Could not close the active workout. Try again.")
    assertEquals(revocations.get(), 0)
    assertEquals(invalidations.get(), 0)
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
      override def invalidateAll(email: String): Task[Unit] = invalidateEffect(email)

  private def workoutSessions(closeEffect: String => Task[Unit]): WorkoutSessions =
    new WorkoutSessions:
      override def closeOnLogout(email: String, at: Instant): Task[Unit] = closeEffect(email)

  private def googleOAuth(revokeEffect: String => Task[Unit]): GoogleOAuth =
    new GoogleOAuth:
      override def authorizationUrl(state: String): UIO[String] = ZIO.succeed("")
      override def authenticate(code: String): Task[GoogleAuthentication] = ZIO.fail(new UnsupportedOperationException)
      override def authenticateIdToken(idToken: String): Task[GoogleAuthentication] =
        ZIO.fail(new UnsupportedOperationException)
      override def callbackIsSecure: UIO[Boolean] = ZIO.succeed(true)
      override def accessToken(refreshToken: String): Task[String] = ZIO.fail(new UnsupportedOperationException)
      override def revoke(refreshToken: String): Task[Unit] = revokeEffect(refreshToken)

  test("raises the logout event once the session is really gone"):
    val events = new AtomicReference(List.empty[LogoutEvent])
    val user = SessionUser("jane@example.com", "Jane", Some("refresh-token"))
    val store = sessionStore(user, _ => ZIO.unit)
    val activeRoutes = routes(store, googleOAuth(_ => ZIO.unit), recordingNotifier(events))

    val response = run(ZIO.scoped(activeRoutes.runZIO(request)))

    assertEquals(response.status, Status.NoContent)
    assertEquals(events.get().map(_.user.email), List("jane@example.com"))
    assertEquals(events.get().map(_.sessionKey), List("session-key"))

  test("raises no logout event when the session could not be invalidated"):
    val events = new AtomicReference(List.empty[LogoutEvent])
    val user = SessionUser("jane@example.com", "Jane", Some("refresh-token"))
    val failingStore = sessionStore(user, _ => ZIO.fail(new RuntimeException("firestore is down")))
    val activeRoutes = routes(failingStore, googleOAuth(_ => ZIO.unit), recordingNotifier(events))

    val response = run(ZIO.scoped(activeRoutes.runZIO(request)))

    // A listener must never record an ending that did not actually happen.
    assertEquals(response.status, Status.ServiceUnavailable)
    assertEquals(events.get(), List.empty[LogoutEvent])
