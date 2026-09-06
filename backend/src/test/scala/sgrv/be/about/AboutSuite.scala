package sgrv.be.about

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import sgrv.api.AboutInfo
import sgrv.be.auth.{SessionStore, SessionUser}
import sgrv.be.core.{CapabilityRegistry, PluginStatus, RouteDiscovery}
import zio.{Runtime, Task, Unsafe, ZEnvironment, ZIO}
import zio.http.{Cookie, Header, Request, Status, URL}
import zio.json.*

class AboutSuite extends munit.FunSuite:
  private def run[A](effect: Task[A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
    }

  test("loads complete, parseable generated build information"):
    val information = BuildInformation.current

    Instant.parse(information.buildDate)
    Seq(
      information.appVersion,
      information.buildOs,
      information.scalaVersion,
      information.scalaJsVersion
    ).foreach(value => assert(value.nonEmpty))

  test("serves build information only after resolving an authenticated session"):
    val lookups = new AtomicInteger(0)
    val store = new SessionStore:
      override def create(
          sessionKey: String,
          user: SessionUser,
          createdAt: Instant,
          expiresAt: Instant
      ): Task[Unit] = ZIO.unit

      override def find(sessionKey: String, now: Instant): Task[Option[SessionUser]] =
        ZIO.succeed:
          lookups.incrementAndGet()
          Option.when(sessionKey == "valid-session")(SessionUser("jane@example.com", "Jane"))

      override def findForRefresh(sessionKey: String): Task[Option[SessionUser]] = ZIO.none
      override def renew(sessionKey: String, expiresAt: Instant): Task[Unit] = ZIO.unit
      override def invalidate(sessionKey: String): Task[Unit] = ZIO.unit

    val registry = CapabilityRegistry.fromEnvironment(ZEnvironment(store))
    val routes = RouteDiscovery.activate(About, About.getClass.getName, registry) match
      case PluginStatus.Active(_, _, activeRoutes) => activeRoutes
      case other                                   => fail(s"Expected About to activate, got $other")

    val signedOut = run(ZIO.scoped(routes.runZIO(Request.get(URL.decode("/about").toOption.get))))
    val signedInRequest = Request
      .get(URL.decode("/about").toOption.get)
      .addCookie(Cookie.Request("session", "valid-session"))
    val signedIn = run(ZIO.scoped(routes.runZIO(signedInRequest)))
    val information = run(signedIn.body.asString).fromJson[AboutInfo]

    assertEquals(signedOut.status, Status.Unauthorized)
    assertEquals(signedIn.status, Status.Ok)
    assertEquals(information, Right(BuildInformation.current))
    assertEquals(signedIn.headers.get(Header.CacheControl), Some(Header.CacheControl.NoStore))
    assertEquals(lookups.get(), 1)
