package sgrv.be

import sgrv.api.CountingSession
import sgrv.be.auth.SessionUser
import sgrv.be.core.*
import zio.*
import zio.http.{Cookie, Request, URL}
import zio.json.ast.Json

object GreetingContributor extends CurrentUserContributor:
  type Requires = Any

  override val id = "test-greeting"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
  override def contribute(context: RequestContext.Authenticated): ZIO[Requires, Throwable, Option[Json]] =
    ZIO.succeed(Some(Json.Obj("hello" -> Json.Str(context.user.email))))

object SilentContributor extends CurrentUserContributor:
  type Requires = Any

  override val id = "test-silent"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
  override def contribute(context: RequestContext.Authenticated): ZIO[Requires, Throwable, Option[Json]] =
    ZIO.none

object ExplodingContributor extends CurrentUserContributor:
  type Requires = Any

  override val id = "test-exploding"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
  override def contribute(context: RequestContext.Authenticated): ZIO[Requires, Throwable, Option[Json]] =
    ZIO.fail(new IllegalStateException("contributor exploded"))

class CurrentUserContributorSuite extends munit.FunSuite:

  private def run[A](effect: ZIO[Any, Any, A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe
        .run(effect.mapError(error => new RuntimeException(error.toString)))
        .getOrThrowFiberFailure()
    }

  private val user = SessionUser("jane@example.com", "Jane")

  private def context(sessionKey: Option[String] = None): RequestContext.Authenticated =
    val base = Request.get(URL.decode("/me").toOption.get)
    val request = sessionKey.fold(base)(key => base.addCookie(Cookie.Request("session", key)))
    RequestContext.Authenticated(request, user)

  private def activate(contributor: CurrentUserContributor): ContributorStatus =
    CurrentUserContributors.activate(contributor, contributor.getClass.getName, CapabilityRegistry.empty)

  test("files each contribution under the contributor's own id"):
    val details = CurrentUserContributors.fromStatuses(Seq(activate(GreetingContributor)))

    assertEquals(
      run(details.extras(context())),
      Map("test-greeting" -> Json.Obj("hello" -> Json.Str("jane@example.com")))
    )

  test("a contributor that declines adds no key at all"):
    val details = CurrentUserContributors.fromStatuses(Seq(activate(SilentContributor)))

    assertEquals(run(details.extras(context())), Map.empty[String, Json])

  test("a failing contributor omits only its own key and never fails /me"):
    val statuses = Seq(activate(ExplodingContributor), activate(GreetingContributor))
    val details = CurrentUserContributors.fromStatuses(statuses)

    assertEquals(run(details.extras(context())).keySet, Set("test-greeting"))

  test("rejects an invalid id and an incompatible contributor API"):
    object BadId extends CurrentUserContributor:
      type Requires = Any
      override val id = "Not Valid"
      override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
      override def contribute(context: RequestContext.Authenticated): ZIO[Requires, Throwable, Option[Json]] =
        ZIO.none

    object Incompatible extends CurrentUserContributor:
      type Requires = Any
      override val id = "test-incompatible"
      override val apiVersion = CurrentUserContributor.ApiVersion + 1
      override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
      override def contribute(context: RequestContext.Authenticated): ZIO[Requires, Throwable, Option[Json]] =
        ZIO.none

    assert(activate(BadId).isInstanceOf[ContributorStatus.Rejected])
    assert(activate(Incompatible).isInstanceOf[ContributorStatus.Rejected])

  test("discovers the counting-session contributor on the real classpath"):
    val discovered = run(CurrentUserContributors.discover(CapabilityRegistry.empty))
    val ids = discovered.collect:
      case ContributorStatus.Active(id, _, _)  => id
      case ContributorStatus.Skipped(id, _, _) => id

    assert(ids.contains(CountingSession.Key), s"Expected the counting-session contributor among $ids")
