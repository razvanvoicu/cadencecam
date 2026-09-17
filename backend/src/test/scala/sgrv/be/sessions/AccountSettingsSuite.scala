package sgrv.be.sessions

import sgrv.api.{AccountSettings, CountsBy, ExerciseType, WeightUnit}
import sgrv.be.core.{CapabilityRegistry, PluginStatus, RouteDiscovery}
import zio.*
import zio.http.{Method, Path}
import zio.json.*

class AccountSettingsSuite extends munit.FunSuite:

  private def run[A](effect: ZIO[Any, Any, A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe
        .run(effect.mapError(error => new RuntimeException(error.toString)))
        .getOrThrowFiberFailure()
    }

  private val cycling = ExerciseType("cycle", "Stationary bicycle", Some("Life Fitness"), CountsBy.Frequency, 1.8)

  private val settings =
    AccountSettings(
      weightKilograms = 78.5,
      weightUnit = WeightUnit.Kilograms,
      defaultFactor = 1.0,
      exercises = Seq(cycling),
      selected = Some("cycle")
    )

  test("settings round-trip through the shape both ends agree on"):
    assertEquals(AccountSettingsRoute.parse(settings.toJson), Right(settings))

  test("an account that has never said anything has settings, and they are the ones it started with"):
    assertEquals(AccountSettings.Initial.countsBy, CountsBy.RepCount)
    assert(AccountSettings.Initial.weightKilograms > 0)

  test("the starting factor lands a half-hour set in the range a half-hour set actually costs"):
    // Calibrated rather than left at one, which reported the formula's raw product -- some tens of thousands of
    // "calories" for half an hour. The reference session: 1000 reps in 31.5 minutes at 98 kg, which a stair machine
    // at top sustainable effort puts at eleven to thirteen METs, so 594 to 702 kcal.
    val cadenceSum = 999 / (31.5 * 60 / 1000.0)
    val reported = cadenceSum * 98.0 * AccountSettings.Initial.defaultFactor / AccountSettings.Divisor

    assert(reported > 550 && reported < 750, f"the reference session reports $reported%.0f kcal")

  test("a factor sits on a scale a person can read and step through"):
    // The hundred in the formula is what puts it there. Without it the same calibration is 0.0125: correct, and
    // impossible to nudge or compare.
    assert(AccountSettings.Initial.defaultFactor >= 0.5 && AccountSettings.Initial.defaultFactor <= 2.0)
    assertEqualsDouble(AccountSettings.FactorStep, 0.05, 1e-9)

  test("a weight or a factor of zero is refused rather than stored"):
    // Either of them makes every figure on the dashboard zero for ever, which reads as the counter being broken
    // rather than as a setting somebody typed.
    assert(AccountSettingsRoute.parse(settings.copy(weightKilograms = 0.0).toJson).isLeft)
    assert(AccountSettingsRoute.parse(settings.copy(defaultFactor = 0.0).toJson).isLeft)
    assert(AccountSettingsRoute.parse(settings.copy(weightKilograms = -80.0).toJson).isLeft)
    assert(AccountSettingsRoute.parse(settings.copy(exercises = Seq(cycling.copy(factor = 0.0))).toJson).isLeft)

  test("two exercises cannot share an identity, which is what the selection is by"):
    val twice = settings.copy(exercises = Seq(cycling, cycling.copy(name = "Something else")))

    assert(AccountSettingsRoute.parse(twice.toJson).isLeft)

  test("an exercise with no name is refused, since nothing on the dashboard could say which it was"):
    assert(AccountSettingsRoute.parse(settings.copy(exercises = Seq(cycling.copy(name = "  "))).toJson).isLeft)

  test("a selection naming an exercise that is not there is dropped rather than kept"):
    // Kept, it would leave the dashboard reporting the default factor while the settings screen showed a chosen row.
    val stale = settings.copy(selected = Some("deleted"))

    assertEquals(AccountSettingsRoute.parse(stale.toJson).map(_.selected), Right(None))

  test("a body that is not settings is refused rather than guessed at"):
    assert(AccountSettingsRoute.parse("").isLeft)
    assert(AccountSettingsRoute.parse("""{"weightKilograms":"heavy"}""").isLeft)
    // The shared type forbids extra fields, so the two ends cannot silently disagree about the shape.
    assert(AccountSettingsRoute.parse("""{"weightKilograms":80,"height":180}""").isLeft)

  test("a weight is stored in kilograms whatever it is entered and shown in"):
    // Otherwise looking at one's weight in pounds would multiply every calorie figure by 2.2, which would be the
    // dashboard reporting the unit rather than the effort.
    val entered = WeightUnit.toKilograms(173.0, WeightUnit.Pounds)

    assertEqualsDouble(entered, 78.47, 0.01)
    assertEqualsDouble(WeightUnit.show(entered, WeightUnit.Pounds), 173.0, 1e-9)
    assertEqualsDouble(WeightUnit.show(entered, WeightUnit.Kilograms), entered, 1e-9)

  test("the settings route is discovered on the classpath, and asks the host for Firestore"):
    val statuses = run(RouteDiscovery.discover(CapabilityRegistry.empty))

    val skipped = statuses.collectFirst:
      case PluginStatus.Skipped(AccountSettingsRoute.id, _, missing) => missing.map(_.id).toSet

    assertEquals(skipped, Some(Set("firestore", "session-store")))

  test("the settings are both read and written on the same path"):
    val patterns = AccountSettingsRoute.routes.routes.map(_.routePattern)

    assert(patterns.exists(_.matches(Method.GET, Path(AccountSettings.Path))), "read")
    assert(patterns.exists(_.matches(Method.PUT, Path(AccountSettings.Path))), "written")
