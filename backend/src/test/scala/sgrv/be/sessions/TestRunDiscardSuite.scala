package sgrv.be.sessions

import sgrv.api.DiscardRun
import sgrv.be.core.{AccessPolicy, CapabilityRegistry, PluginStatus, RouteDiscovery}
import zio.*
import zio.json.*

class TestRunDiscardSuite extends munit.FunSuite:

  private def run[A](effect: ZIO[Any, Any, A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe
        .run(effect.mapError(error => new RuntimeException(error.toString)))
        .getOrThrowFiberFailure()
    }

  test("the discard route is discovered on the classpath"):
    // A plugin can compile perfectly and never be reached by the scan, in which case the bench's Abandon would post
    // into a 404 and report nothing -- looking, from the bench, exactly like a discard that worked.
    val statuses = run(RouteDiscovery.discover(CapabilityRegistry.empty))

    val skipped = statuses.collectFirst:
      case PluginStatus.Skipped(TestRunDiscards.id, _, missing) => missing.map(_.id).toSet

    assertEquals(skipped, Some(Set("firestore", "session-store")))

  test("the route is claimed where the frontend posts it"):
    // The path lives in shared code precisely so the two ends cannot drift; this asserts they have not.
    assertEquals(DiscardRun.Path, "/test/run/discard")

  test("the route is closed to anyone not signed in"):
    // The run id is not a secret -- it travels in the note of every recording -- so the only thing between a known run
    // id and someone else's recordings is that the caller must be signed in, and the handler scopes every query to
    // their own email. The policy is what the host enforces before the handler is ever reached.
    assertEquals(TestRunDiscards.accessPolicy, AccessPolicy.Authenticated)

  test("a run to discard has to be named"):
    assertEquals(DiscardRun("run-1").toJson.fromJson[DiscardRun], Right(DiscardRun("run-1")))
    assert("""{"runId":"run-1","extra":1}""".fromJson[DiscardRun].isLeft)
