package sgrv.be.sessions

import sgrv.api.{PeerRole, PeerSignal, PeerSignals}
import sgrv.be.core.{AccessPolicy, CapabilityRegistry, PluginStatus, RouteDiscovery}
import zio.*
import zio.json.*

class PeerSignallingSuite extends munit.FunSuite:

  private def run[A](effect: ZIO[Any, Any, A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe
        .run(effect.mapError(error => new RuntimeException(error.toString)))
        .getOrThrowFiberFailure()
    }

  test("the signalling route is discovered on the classpath"):
    // A plugin can compile and never be scanned, in which case both browsers post into a 404 and simply never pair.
    val statuses = run(RouteDiscovery.discover(CapabilityRegistry.empty))

    val skipped = statuses.collectFirst:
      case PluginStatus.Skipped(PeerSignalling.id, _, missing) => missing.map(_.id).toSet

    assertEquals(skipped, Some(Set("firestore", "session-store")))

  test("the route is claimed where the browsers post it"):
    assertEquals(PeerSignal.Path, "/live/signal")

  test("only a signed-in caller may use the post box"):
    // Both ends resolve the session from being signed in, so nothing in a request says whose exchange it is.
    assertEquals(PeerSignalling.accessPolicy, AccessPolicy.Authenticated)

  test("a signal survives the round trip through JSON"):
    val signal = PeerSignal(PeerRole.Watcher, "offer", """{"type":"offer","sdp":"v=0…"}""")

    assertEquals(signal.toJson.fromJson[PeerSignal], Right(signal))
    assert("""{"from":"Watcher","kind":"offer","body":"b","extra":1}""".fromJson[PeerSignal].isLeft)

  test("an empty post box reads as empty rather than as a failure"):
    assertEquals(PeerSignals().toJson.fromJson[PeerSignals], Right(PeerSignals(Seq.empty, "")))

  test("the watcher offers and the counter answers"):
    assertEquals(PeerRole.values.map(_.toString).toSeq, Seq("Watcher", "Counter"))
