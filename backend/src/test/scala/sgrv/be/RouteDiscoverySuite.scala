package sgrv.be

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import sgrv.be.auth.{GoogleAuthentication, GoogleOAuth, SessionStore, SessionUser}
import sgrv.be.core.*
import sgrv.be.sheets.SpreadsheetContent
import zio.*
import zio.http.{Client, Cookie, Method, Path, Request, Response, Routes, Status, URL, handler}

object EchoPlugin extends BackendPlugin:
  type Requires = Any

  override val id = "test-echo"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override val routes: Routes[Requires & RequestContext, Nothing] = Routes(
    Method.POST / "echo" -> handler((request: Request) => Response.text(s"${request.method.name}:${request.url.path}")),
    Method.PUT / "echo" -> handler((request: Request) => Response.text(s"${request.method.name}:${request.url.path}"))
  )

object ProtectedEchoPlugin extends BackendPlugin:
  type Requires = SessionStore

  override val id = "test-protected-echo"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(
      Method.GET / "protected-echo" -> handler:
        ZIO.serviceWith[RequestContext]:
          case RequestContext.Authenticated(_, user) => Response.text(user.email)
          case RequestContext.Public(_)              => Response.status(Status.InternalServerError)
    )

object IncompatiblePlugin extends BackendPlugin:
  type Requires = Any

  override val id = "test-incompatible"
  override val apiVersion = BackendPlugin.ApiVersion + 1
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override val routes: Routes[Requires & RequestContext, Nothing] = Routes.empty

object FailedPlugin extends BackendPlugin:
  type Requires = Any

  override val id = "test-failed"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override def routes: Routes[Requires & RequestContext, Nothing] =
    throw new IllegalStateException("activation failed")

object InvalidPlugin

trait TestAlpha:
  def value: String

trait TestBeta:
  def value: String

class RouteDiscoverySuite extends munit.FunSuite:
  private val sessionStore: SessionStore = new SessionStore:
    override def create(
        sessionKey: String,
        user: SessionUser,
        createdAt: Instant,
        expiresAt: Instant
    ): Task[Unit] = ZIO.unit

    override def find(sessionKey: String, now: Instant): Task[Option[SessionUser]] = ZIO.none

    override def findForRefresh(sessionKey: String): Task[Option[SessionUser]] = ZIO.none
    override def renew(sessionKey: String, expiresAt: Instant): Task[Unit] = ZIO.unit
    override def invalidate(sessionKey: String): Task[Unit] = ZIO.unit

  private val sessionRegistry = CapabilityRegistry.fromEnvironment(ZEnvironment(sessionStore))

  private def run[A](effect: Task[A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
    }

  test("loads a nominal plugin object and activates its native routes"):
    val loaded = run(RouteDiscovery.load(EchoPlugin.getClass.getName, getClass.getClassLoader))
    val status = RouteDiscovery.activate(loaded, EchoPlugin.getClass.getName, CapabilityRegistry.empty)
    val routes = RouteDiscovery.fromStatuses(Seq(status))
    val request = Request(method = Method.POST, url = URL.decode("/echo").toOption.get)
    val response = run(ZIO.scoped(routes.runZIO(request)))

    assertEquals(loaded.id, "test-echo")
    assertEquals(run(response.body.asString), "POST:/echo")
    assert(routes.routes.exists(_.routePattern.matches(Method.PUT, Path("/echo"))))

  test("skips a plugin when its typed capability requirements cannot be resolved"):
    val status = RouteDiscovery.activate(
      ProtectedEchoPlugin,
      ProtectedEchoPlugin.getClass.getName,
      CapabilityRegistry.empty
    )

    status match
      case PluginStatus.Skipped(id, _, missing) =>
        assertEquals(id, "test-protected-echo")
        assertEquals(missing.map(_.id), Chunk("session-store"))
      case other => fail(s"Expected a skipped plugin, got $other")

  test("closes an activated plugin over the resolved environment and applies its access policy"):
    val status = RouteDiscovery.activate(ProtectedEchoPlugin, ProtectedEchoPlugin.getClass.getName, sessionRegistry)
    val routes = RouteDiscovery.fromStatuses(Seq(status))
    val request = Request.get(URL.decode("/protected-echo").toOption.get)
    val response = run(ZIO.scoped(routes.runZIO(request)))

    assertEquals(response.status, Status.Unauthorized)

  test("resolves an authenticated session once and supplies its user to the route context"):
    val lookups = new AtomicInteger(0)
    val user = SessionUser("jane@example.com", "Jane", Some("refresh-token"))
    val countingStore: SessionStore = new SessionStore:
      override def create(
          sessionKey: String,
          user: SessionUser,
          createdAt: Instant,
          expiresAt: Instant
      ): Task[Unit] = ZIO.unit

      override def find(sessionKey: String, now: Instant): Task[Option[SessionUser]] =
        ZIO.succeed:
          lookups.incrementAndGet()
          Option.when(sessionKey == "session-key")(user)

      override def findForRefresh(sessionKey: String): Task[Option[SessionUser]] = ZIO.none
      override def renew(sessionKey: String, expiresAt: Instant): Task[Unit] = ZIO.unit
      override def invalidate(sessionKey: String): Task[Unit] = ZIO.unit

    val registry = CapabilityRegistry.fromEnvironment(ZEnvironment(countingStore))
    val status = RouteDiscovery.activate(ProtectedEchoPlugin, ProtectedEchoPlugin.getClass.getName, registry)
    val routes = RouteDiscovery.fromStatuses(Seq(status))
    val request = Request
      .get(URL.decode("/protected-echo").toOption.get)
      .addCookie(Cookie.Request("session", "session-key"))
    val response = run(ZIO.scoped(routes.runZIO(request)))

    assertEquals(response.status, Status.Ok)
    assertEquals(run(response.body.asString), user.email)
    assertEquals(lookups.get(), 1)

  test("an authenticated Sheets request reads its Firestore session only once"):
    val lookups = new AtomicInteger(0)
    val user = SessionUser("jane@example.com", "Jane")
    val countingStore: SessionStore = new SessionStore:
      override def create(
          sessionKey: String,
          user: SessionUser,
          createdAt: Instant,
          expiresAt: Instant
      ): Task[Unit] = ZIO.unit

      override def find(sessionKey: String, now: Instant): Task[Option[SessionUser]] =
        ZIO.succeed:
          lookups.incrementAndGet()
          Option.when(sessionKey == "session-key")(user)

      override def findForRefresh(sessionKey: String): Task[Option[SessionUser]] = ZIO.none
      override def renew(sessionKey: String, expiresAt: Instant): Task[Unit] = ZIO.unit
      override def invalidate(sessionKey: String): Task[Unit] = ZIO.unit

    val googleOAuth: GoogleOAuth = new GoogleOAuth:
      override def authorizationUrl(state: String): UIO[String] = ZIO.succeed("")
      override def authenticate(code: String): Task[GoogleAuthentication] =
        ZIO.fail(new UnsupportedOperationException)
      override def callbackIsSecure: UIO[Boolean] = ZIO.succeed(true)
      override def accessToken(refreshToken: String): Task[String] =
        ZIO.fail(new AssertionError("Sheets must reject the missing refresh token before requesting an access token"))
      override def revoke(refreshToken: String): Task[Unit] = ZIO.unit

    RouteDiscovery.activate(
      SpreadsheetContent,
      SpreadsheetContent.getClass.getName,
      CapabilityRegistry.fromEnvironment(ZEnvironment(countingStore, googleOAuth))
    ) match
      case PluginStatus.Skipped(_, _, missing) => assertEquals(missing.map(_.id), Chunk("http-client"))
      case other => fail(s"Expected the generic HTTP capability to be missing, got $other")

    val response = run:
      (for
        httpClient <- ZIO.service[Client]
        registry = CapabilityRegistry.fromEnvironment(ZEnvironment(countingStore, googleOAuth, httpClient))
        status = RouteDiscovery.activate(SpreadsheetContent, SpreadsheetContent.getClass.getName, registry)
        routes = RouteDiscovery.fromStatuses(Seq(status))
        request = Request
          .get(URL.decode("/sheets/content?name=Budget").toOption.get)
          .addCookie(Cookie.Request("session", "session-key"))
        response <- ZIO.scoped(routes.runZIO(request))
      yield response).provide(Client.default)

    assertEquals(response.status, Status.Forbidden)
    assertEquals(lookups.get(), 1)

  test("resolves a composed capability set with its intersection type intact"):
    val alpha: TestAlpha = new TestAlpha:
      override val value = "alpha"
    val beta: TestBeta = new TestBeta:
      override val value = "beta"
    val alphaCapability = Capability[TestAlpha]("test-alpha")
    val betaCapability = Capability[TestBeta]("test-beta")
    val registry = CapabilityRegistry.fromEnvironment(ZEnvironment(alpha, beta))
    val plugin = new BackendPlugin:
      type Requires = TestAlpha & TestBeta

      override val id = "test-composed-environment"
      override val requirements: CapabilitySet[Requires] =
        CapabilitySet.one(alphaCapability) ++ CapabilitySet.one(betaCapability)
      override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
      override val routes: Routes[Requires & RequestContext, Nothing] = Routes(
        Method.GET / "composed" -> handler:
          for
            a <- ZIO.service[TestAlpha]
            b <- ZIO.service[TestBeta]
          yield Response.text(s"${a.value}:${b.value}")
      )
    val status = RouteDiscovery.activate(plugin, "ComposedPlugin", registry)
    val routes = RouteDiscovery.fromStatuses(Seq(status))
    val response = run(ZIO.scoped(routes.runZIO(Request.get(URL.decode("/composed").toOption.get))))
    val missing = RouteDiscovery.activate(plugin, "ComposedPlugin", CapabilityRegistry.empty)

    assertEquals(run(response.body.asString), "alpha:beta")
    missing match
      case PluginStatus.Skipped(_, _, capabilities) =>
        assertEquals(capabilities.map(_.id), Chunk("test-alpha", "test-beta"))
      case other => fail(s"Expected both missing capabilities, got $other")

  test("rejects a plugin compiled for an incompatible plugin API"):
    val status =
      RouteDiscovery.activate(IncompatiblePlugin, IncompatiblePlugin.getClass.getName, CapabilityRegistry.empty)

    status match
      case PluginStatus.Rejected(_, reason) => assert(reason.contains("API version"), reason)
      case other                            => fail(s"Expected a rejected plugin, got $other")

  test("isolates a plugin that throws while constructing its routes"):
    val status = RouteDiscovery.activate(FailedPlugin, FailedPlugin.getClass.getName, CapabilityRegistry.empty)

    status match
      case PluginStatus.Failed(id, _, cause) =>
        assertEquals(id, "test-failed")
        assertEquals(cause.getMessage, "activation failed")
      case other => fail(s"Expected a failed plugin, got $other")

  test("rejects every plugin involved in an overlapping route pattern"):
    val first = PluginStatus.Active(
      "first",
      "First",
      Routes(Method.GET / "collision" -> handler(Response.ok))
    )
    val second = PluginStatus.Active(
      "second",
      "Second",
      Routes(Method.GET / "collision" -> handler(Response.ok))
    )
    val statuses = RouteDiscovery.rejectConflicts(Seq(first, second))

    assert(statuses.forall(_.isInstanceOf[PluginStatus.Rejected]), statuses)
    assertEquals(RouteDiscovery.fromStatuses(statuses).routes.size, 0)

  test("rejects every active plugin sharing the same stable id"):
    val first = PluginStatus.Active("duplicate", "First", Routes.empty)
    val second = PluginStatus.Active("duplicate", "Second", Routes.empty)
    val statuses = RouteDiscovery.rejectDuplicateIds(Seq(first, second))

    assert(statuses.forall(_.isInstanceOf[PluginStatus.Rejected]), statuses)

  test("rejects a dynamically requested route pattern reserved by the static application"):
    val active = PluginStatus.Active(
      "reserved",
      "Reserved",
      Routes(Method.GET / "index.html" -> handler(Response.ok))
    )
    val statuses = RouteDiscovery.rejectConflicts(Seq(active), Set(Method.GET / "index.html"))

    assert(statuses.head.isInstanceOf[PluginStatus.Rejected], statuses)

  test("rejects a module that does not implement the nominal plugin contract"):
    val result = run(RouteDiscovery.load(InvalidPlugin.getClass.getName, getClass.getClassLoader).either)

    result match
      case Left(error: IllegalArgumentException) => assert(error.getMessage.contains("does not implement"), error)
      case Left(error) => fail(s"Expected IllegalArgumentException, got ${error.getClass.getName}: ${error.getMessage}")
      case Right(value) => fail(s"Expected plugin loading to fail, got $value")

  test("discovers plugins independently and keeps their activation outcomes"):
    val statuses = run(RouteDiscovery.discover(sessionRegistry))

    assert(statuses.exists {
      case PluginStatus.Active("test-echo", _, _) => true
      case _                                      => false
    })
    assert(statuses.exists {
      case PluginStatus.Rejected(className, reason) =>
        className.contains("IncompatiblePlugin") && reason.contains("API version")
      case _ => false
    })
    assert(statuses.exists {
      case PluginStatus.Failed("test-failed", _, _) => true
      case _                                        => false
    })
