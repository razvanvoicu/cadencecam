package sgrv.be.sessions

import sgrv.api.{AccountData, DiscardWorkout, RepProgress, Workout, WorkoutHistory}
import sgrv.be.core.{CapabilityRegistry, PluginStatus, RouteDiscovery}
import zio.*
import zio.http.{Method, Path}
import zio.json.*

class WorkoutHistorySuite extends munit.FunSuite:

  private def run[A](effect: ZIO[Any, Any, A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe
        .run(effect.mapError(error => new RuntimeException(error.toString)))
        .getOrThrowFiberFailure()
    }

  test("a workout is finished when it has an end, and running until then"):
    // What the history marks rather than hides. The running one is the workout the dashboard's own figures belong to,
    // and leaving it off the list would read as the app having lost it.
    val running = Workout(id = "w1", startedAtMillis = 1.0, reps = 12)

    assert(running.running)
    assert(!running.copy(endedAtMillis = Some(2.0)).running)

  test("a history round-trips through the shape both ends agree on"):
    val history = WorkoutHistory(
      Seq(
        Workout("w1", 1_700_000_000_000.0, Some(1_700_000_900_000.0), 247, 130.5, 900.0, Some("LoggedOut")),
        Workout("w2", 1_699_000_000_000.0)
      )
    )

    assertEquals(history.toJson.fromJson[WorkoutHistory], Right(history))

  test("a report carries the accumulator the history needs, and old reports still parse"):
    // The figure is not stored, only what it is worked out from: a weight and a factor can change afterwards, and a
    // calorie total computed under an old factor and kept would be a number nothing else on the screen agreed with.
    val full = """{"reps":247,"cadenceSum":130.5,"elapsedSeconds":900.0}"""

    assertEquals(CountingSessionProgress.reported(full), Right(RepProgress(247, 130.5, 900.0)))
    // A counter that predates the fields reports neither, and still counts.
    assertEquals(CountingSessionProgress.reported("""{"reps":57}"""), Right(RepProgress(57)))
    assertEquals(CountingSessionProgress.reps(full), Right(247))

  test("a report of something that is not a measurement is refused rather than filed"):
    assert(CountingSessionProgress.reported("""{"reps":-1}""").isLeft)
    assert(CountingSessionProgress.reported("""{"reps":10,"cadenceSum":-3.0}""").isLeft)
    assert(CountingSessionProgress.reported("""{"reps":10,"elapsedSeconds":-1.0}""").isLeft)
    // The shared type forbids extra fields, so the two ends cannot silently disagree about the shape.
    assert(CountingSessionProgress.reported("""{"reps":10,"calories":400}""").isLeft)

  test("the history is read and one workout discarded on the paths both ends name"):
    val patterns = WorkoutHistoryRoute.routes.routes.map(_.routePattern)

    assert(patterns.exists(_.matches(Method.GET, Path(WorkoutHistory.Path))), "read")
    assert(patterns.exists(_.matches(Method.POST, Path(DiscardWorkout.Path))), "discarded")

  test("the history route is discovered on the classpath, and asks the host for Firestore"):
    val statuses = run(RouteDiscovery.discover(CapabilityRegistry.empty))

    val skipped = statuses.collectFirst:
      case PluginStatus.Skipped(WorkoutHistoryRoute.id, _, missing) => missing.map(_.id).toSet

    assertEquals(skipped, Some(Set("firestore", "session-store")))

  test("a history is bounded, so one request cannot ask for a database"):
    assert(WorkoutHistory.Limit > 0 && WorkoutHistory.Limit <= 1000)

  test("everything an account holds is removed by one route, which takes no argument"):
    // No id to get wrong, and none that could name somebody else's account: what is removed is whoever is signed in.
    val patterns = AccountDataRoute.routes.routes.map(_.routePattern)

    assertEquals(patterns.size, 1)
    assert(patterns.exists(_.matches(Method.DELETE, Path(AccountData.Path))))

  test("the delete-everything route is discovered on the classpath, and asks the host for Firestore"):
    val statuses = run(RouteDiscovery.discover(CapabilityRegistry.empty))

    val skipped = statuses.collectFirst:
      case PluginStatus.Skipped(AccountDataRoute.id, _, missing) => missing.map(_.id).toSet

    assertEquals(skipped, Some(Set("firestore", "session-store")))
