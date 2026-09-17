package sgrv.fe

import munit.FunSuite
import sgrv.api.{AccountSettings, CountsBy, ExerciseType}

class EffortSuite extends FunSuite:

  private def exercise(countsBy: CountsBy, factor: Double) =
    AccountSettings(
      weightKilograms = 80.0,
      defaultFactor = 1.0,
      exercises = Seq(ExerciseType("cycle", "Stationary bicycle", None, countsBy, factor)),
      selected = Some("cycle")
    )

  test("the horizon is the next round half hour the workout has not reached"):
    assertEquals(Effort.boundaryMinutes(0.0), 30)
    assertEquals(Effort.boundaryMinutes(12.7), 30)
    assertEquals(Effort.boundaryMinutes(29.99), 30)

  test("passing a boundary moves the horizon to the next one rather than leaving it behind"):
    // The figure this labels stops being a projection the moment the clock reaches it: "at 30 min" while the clock
    // says thirty-four is the present tense with the wrong caption on it.
    assertEquals(Effort.boundaryMinutes(30.0), 60)
    assertEquals(Effort.boundaryMinutes(44.0), 60)
    assertEquals(Effort.boundaryMinutes(60.0), 90)
    assertEquals(Effort.boundaryMinutes(91.0), 120)

  test("a horizon is said in minutes up to the hour and in hours after it"):
    assertEquals(Effort.boundaryLabel(30), "30 min")
    assertEquals(Effort.boundaryLabel(60), "1 h")
    assertEquals(Effort.boundaryLabel(90), "1.5 h")
    assertEquals(Effort.boundaryLabel(120), "2 h")
    assertEquals(Effort.boundaryLabel(150), "2.5 h")

  test("a clock that has gone wrong produces a caption rather than an overflow"):
    assert(Effort.boundaryMinutes(Double.NaN) > 0)
    assert(Effort.boundaryMinutes(-5.0) > 0)
    assert(Effort.boundaryMinutes(1e12) > 0)

  test("a rate needs a clock to divide by"):
    assertEquals(Effort.perMinute(100.0, 0.0), None)
    assertEquals(Effort.perMinute(100.0, 4.0), Some(25.0))

  test("the projection is the pace so far carried to the horizon"):
    // Two hundred reps in twenty minutes is ten a minute, so half an hour of it is three hundred.
    assertEquals(Effort.atBoundary(200.0, 20.0), Some(300.0))
    // Past the half hour the same pace is carried to the hour instead.
    assertEquals(Effort.atBoundary(400.0, 40.0), Some(600.0))

  test("the projection and the rate beside it are the same statement"):
    // A dashboard showing a rate and a projection that do not imply each other invites arithmetic that fails, so
    // the projection is the total plus the shown rate over the time left, by construction.
    val total = 247.0
    val minutes = 12.0
    val rate = Effort.perMinute(total, minutes).get
    val horizon = Effort.boundaryMinutes(minutes)

    assertEqualsDouble(Effort.atBoundary(total, minutes).get, total + rate * (horizon - minutes), 1e-9)

  test("nothing is extrapolated from the opening seconds of a set"):
    // At four seconds in, a projection to the half hour is a multiplication by four hundred and fifty: it would be a
    // restatement of the first gap between two reps, with two orders of magnitude of confidence attached.
    assertEquals(Effort.atBoundary(2.0, 4.0 / 60.0), None)
    assert(Effort.atBoundary(2.0, Effort.MinimumProjectionSeconds / 60.0).isDefined)

  test("counting by reps multiplies the count by the weight and the factor"):
    assertEqualsDouble(Effort.calories(100, 999.0, exercise(CountsBy.RepCount, 1.5)), 100 * 80.0 * 1.5, 1e-9)

  test("counting by frequency uses the accumulator instead, and ignores the count"):
    assertEqualsDouble(Effort.calories(100, 40.0, exercise(CountsBy.Frequency, 2.0)), 40.0 * 80.0 * 2.0, 1e-9)

  test("an account that has chosen no exercise counts reps at its default factor"):
    val settings = AccountSettings(weightKilograms = 80.0, defaultFactor = 3.0)

    assertEquals(settings.countsBy, CountsBy.RepCount)
    assertEqualsDouble(Effort.calories(10, 5.0, settings), 10 * 80.0 * 3.0, 1e-9)

  test("a selection naming an exercise that is not there falls back rather than reporting nothing"):
    val settings = exercise(CountsBy.Frequency, 2.0).copy(selected = Some("gone"))

    assertEquals(settings.selectedExercise, None)
    assertEquals(settings.countsBy, CountsBy.RepCount)
    assertEqualsDouble(settings.factor, 1.0, 1e-9)

  test("a total is grouped, so its leading digit is not read as something standing on its own"):
    assertEquals(Effort.grouped(1236.0), "1,236")
    assertEquals(Effort.grouped(421.4), "421")
    assertEquals(Effort.grouped(0.0), "0")
    assertEquals(Effort.grouped(1_234_567.0), "1,234,567")

  test("the clock reads as a clock, and grows an hours field only when there is one"):
    assertEquals(Effort.elapsedClock(0.0), "0:00")
    assertEquals(Effort.elapsedClock(761.0), "12:41")
    assertEquals(Effort.elapsedClock(3600.0), "1:00:00")
    assertEquals(Effort.elapsedClock(-5.0), "0:00")
