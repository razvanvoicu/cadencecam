package sgrv.be.auth

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import sgrv.api.MobileSession
import sgrv.be.core.{CapabilityRegistry, LoginEvent, LogoutEvent, PluginStatus, RouteDiscovery, SessionNotifier}
import zio.{Runtime, Task, UIO, Unsafe, ZEnvironment, ZIO}
import zio.http.{Body, Header, Request, Status, URL}
import zio.json.*

class InvitationLoginSuite extends munit.FunSuite:

  private def run[A](effect: Task[A]): A =
    Unsafe.unsafe { implicit unsafe => Runtime.default.unsafe.run(effect).getOrThrowFiberFailure() }

  test("exchanges invitation credentials for the ordinary cookie and bearer session"):
    val created = new AtomicReference(Option.empty[(String, SessionUser)])
    val request = Request.post(
      URL.decode("/auth/invitation").toOption.get,
      Body.fromString("""{"username":"review-one","accessCode":"correct-horse-battery-staple"}""")
    )
    val response = run(ZIO.scoped(routes(created).runZIO(request)))
    val body = run(response.body.asString).fromJson[MobileSession]

    assertEquals(response.status, Status.Ok)
    assertEquals(response.headers.get(Header.CacheControl), Some(Header.CacheControl.NoStore))
    val sessionCookie = response.headers
      .getAll(Header.SetCookie)
      .map(_.value)
      .find(_.name == Callback.sessionCookieName)
    assertEquals(sessionCookie.map(_.content), Some("invitation-session"))
    assertEquals(body.map(_.accessToken), Right("invitation-session"))
    assertEquals(body.map(_.user.email), Right("review-one"))
    assertEquals(created.get().map(_._2.identityProvider), Some(IdentityProvider.Invitation))
    assertEquals(created.get().map(_._2.account), Some("invitation:account-one"))

  test("uses one generic rejection for invalid invitation credentials"):
    val request = Request.post(
      URL.decode("/auth/invitation").toOption.get,
      Body.fromString("""{"username":"review-one","accessCode":"wrong-access-code-value"}""")
    )
    val response = run(ZIO.scoped(routes(new AtomicReference(None), reject = true).runZIO(request)))

    assertEquals(response.status, Status.Unauthorized)
    assertEquals(run(response.body.asString), "The invitation username or access code is not valid.")

  private def routes(
      created: AtomicReference[Option[(String, SessionUser)]],
      reject: Boolean = false
  ) =
    val invitations = new InvitationAccounts:
      override def create(invitations: Seq[NewInvitation], at: Instant) = ZIO.succeed(Seq.empty)
      override def authenticate(username: String, accessCode: String, at: Instant): Task[SessionUser] =
        if reject then ZIO.fail(new IllegalArgumentException("private detail"))
        else
          assertEquals(username, "review-one")
          assertEquals(accessCode, "correct-horse-battery-staple")
          ZIO.succeed(
            SessionUser(
              "review-one",
              "Review one",
              identityProvider = IdentityProvider.Invitation,
              providerSubject = Some("subject-one"),
              accountId = Some("invitation:account-one")
            )
          )
      override def isActive(subject: String): Task[Boolean] = ZIO.succeed(true)
      override def revoke(username: String, at: Instant): Task[Option[String]] = ZIO.none
      override def delete(subject: String, at: Instant): Task[Unit] = ZIO.unit

    val sessions = new SessionStore:
      override def create(key: String, user: SessionUser, createdAt: Instant, expiresAt: Instant): Task[Unit] =
        ZIO.succeed(created.set(Some(key -> user)))
      override def find(sessionKey: String, now: Instant) = ZIO.none
      override def findForRefresh(sessionKey: String) = ZIO.none
      override def renew(sessionKey: String, expiresAt: Instant) = ZIO.unit
      override def invalidate(sessionKey: String) = ZIO.unit
      override def invalidateAll(accountId: String) = ZIO.unit

    val tokens = new TokenGenerator:
      override def generate(bytes: Int): UIO[String] =
        assertEquals(bytes, 32)
        ZIO.succeed("invitation-session")

    val notifier = new SessionNotifier:
      override def loginSucceeded(event: LoginEvent): UIO[Unit] = ZIO.unit
      override def loggedOut(event: LogoutEvent): UIO[Unit] = ZIO.unit

    val google = new GoogleOAuth:
      override def authorizationUrl(state: String) = ZIO.succeed("")
      override def authenticate(code: String) = ZIO.fail(new UnsupportedOperationException)
      override def authenticateIdToken(idToken: String) = ZIO.fail(new UnsupportedOperationException)
      override def callbackIsSecure = ZIO.succeed(true)
      override def accessToken(refreshToken: String) = ZIO.fail(new UnsupportedOperationException)
      override def revoke(refreshToken: String) = ZIO.unit

    val registry = CapabilityRegistry.fromEnvironment(ZEnvironment(invitations, sessions, tokens, notifier, google))
    RouteDiscovery.activate(InvitationLogin, InvitationLogin.getClass.getName, registry) match
      case PluginStatus.Active(_, _, activeRoutes) => activeRoutes
      case other                                   => fail(s"Expected InvitationLogin to activate, got $other")

class InvitationAccountsSuite extends munit.FunSuite:
  test("normalizes safe invitation usernames"):
    assertEquals(InvitationAccounts.validateUsername("  Apple-Review_1 "), "apple-review_1")

  test("rejects usernames that are unsuitable as reviewer credentials"):
    Seq("ab", "has spaces", "slash/name", "x" * 49).foreach: username =>
      intercept[IllegalArgumentException](InvitationAccounts.validateUsername(username))
