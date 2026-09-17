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

  test("the projection carries the current pace over the time that is left"):
    // Two hundred reps at twenty minutes, still going at ten a minute: ten more minutes to the half hour, so three
    // hundred. Only the remainder is projected; the two hundred already counted are not a claim about anything.
    assertEquals(Effort.atBoundary(200.0, 10.0, 20.0), Some(300.0))
    // Past the half hour the horizon is the hour, so the same pace is carried twenty minutes further.
    assertEquals(Effort.atBoundary(400.0, 10.0, 40.0), Some(600.0))

  test("a projection at the boundary itself claims nothing beyond the total"):
    assertEquals(Effort.atBoundary(300.0, 10.0, 30.0 - 1e-9).map(_.round), Some(300L))

  test("the projection moves with the pace beside it, rep by rep"):
    // The point of the change: a dashboard whose projection could only crawl was reporting an average rather than
    // what the exerciser was doing, and a rep taken faster has to show up in both figures at once.
    val steady = Effort.atBoundary(200.0, 10.0, 20.0).get
    val faster = Effort.atBoundary(200.0, 12.0, 20.0).get

    assertEqualsDouble(faster - steady, 20.0, 1e-9)

  private def marks(gapSeconds: Double, count: Int, from: Double = 0.0) =
    (0 until count).map(i => Effort.RepMark(i + 1, from + i * gapSeconds, (i * 1.0 / gapSeconds))).toVector

  test("the pace is the last ten reps, not the whole session"):
    // Eleven marks a second and a half apart: ten reps over fifteen seconds is forty a minute.
    val window = marks(1.5, Effort.PaceWindowReps)

    assertEqualsDouble(Effort.pace(window).get, 40.0, 1e-9)

  test("the pace answers within a rep of the cadence changing"):
    // A window that has just taken in one much faster rep must move, which is what "instantaneous" has to mean.
    val steady = marks(2.0, Effort.PaceWindowReps)
    val quickened = steady.dropRight(1) :+ Effort.RepMark(steady.last.reps, steady.last.atSeconds - 1.0, 0.0)

    assert(Effort.pace(quickened).get > Effort.pace(steady).get * 1.05)

  test("a window is read from its ends, so a batch of reps counted at once is not a slow set"):
    // The detector confirms its first run all at once, so one reading can carry five reps. Counting marks rather
    // than reps would report a fifth of the true pace at the start of every set.
    val batched = Vector(Effort.RepMark(5, 0.0, 0.0), Effort.RepMark(6, 2.0, 0.0), Effort.RepMark(10, 8.0, 0.0))

    assertEqualsDouble(Effort.pace(batched).get, (10 - 5) * 60.0 / 8.0, 1e-9)

  test("a window with nothing to measure over reports nothing rather than a number"):
    assertEquals(Effort.pace(Vector.empty), None)
    assertEquals(Effort.pace(Vector(Effort.RepMark(1, 3.0, 0.0))), None)
    // Two marks at the same instant: a span of zero is not a pace of infinity.
    assertEquals(Effort.pace(Vector(Effort.RepMark(1, 3.0, 0.0), Effort.RepMark(2, 3.0, 0.0))), None)

  test("calories are paced over the same window as the reps"):
    val settings = exercise(CountsBy.Frequency, 1.0)
    val window = Vector(Effort.RepMark(1, 0.0, 0.0), Effort.RepMark(11, 15.0, 8.0))
    val burned = Effort.calories(11, 8.0, settings) - Effort.calories(1, 0.0, settings)

    assertEqualsDouble(Effort.burn(window, settings).get, burned * 60.0 / 15.0, 1e-9)

  test("counting by reps multiplies the count by the weight and the factor, over the hundred"):
    assertEqualsDouble(Effort.calories(100, 999.0, exercise(CountsBy.RepCount, 1.5)), 100 * 80.0 * 1.5 / 100, 1e-9)

  test("counting by frequency uses the accumulator instead, and ignores the count"):
    assertEqualsDouble(Effort.calories(100, 40.0, exercise(CountsBy.Frequency, 2.0)), 40.0 * 80.0 * 2.0 / 100, 1e-9)

  test("the reference session lands where the research put it"):
    // 1000 reps in 31.5 minutes at 98 kg, which a stair machine at top sustainable effort puts at about twelve METs
    // -- 648 kcal. The starting factor is what makes the frequency formula say so.
    val settings = AccountSettings(
      weightKilograms = 98.0,
      exercises = Seq(ExerciseType("stairs", "Stair climber", None, CountsBy.Frequency)),
      selected = Some("stairs")
    )
    val cadenceSum = 999 / (31.5 * 60 / 1000.0)

    assertEqualsDouble(Effort.calories(1000, cadenceSum, settings), 648.0, 10.0)

  test("an account that has chosen no exercise counts reps at its default factor"):
    val settings = AccountSettings(weightKilograms = 80.0, defaultFactor = 3.0)

    assertEquals(settings.countsBy, CountsBy.RepCount)
    assertEqualsDouble(Effort.calories(10, 5.0, settings), 10 * 80.0 * 3.0 / 100, 1e-9)

  test("a selection naming an exercise that is not there falls back rather than reporting nothing"):
    val settings = exercise(CountsBy.Frequency, 2.0).copy(selected = Some("gone"))

    assertEquals(settings.selectedExercise, None)
    assertEquals(settings.countsBy, CountsBy.RepCount)
    assertEqualsDouble(settings.factor, 1.0, 1e-9)

  test("a factor is shown to two decimals, which is the scale it lives on"):
    assertEquals(Effort.factorText(1.25), "1.25")
    assertEquals(Effort.factorText(0.5), "0.50")
    assertEquals(Effort.factorText(2.0), "2.00")

  test("a tap moves a factor one step, and onto the step grid"):
    assertEqualsDouble(Effort.nudged(1.25, up = true), 1.30, 1e-9)
    assertEqualsDouble(Effort.nudged(1.25, up = false), 1.20, 1e-9)
    // A typed value between steps is snapped rather than offset, so a column of factors stays comparable.
    assertEqualsDouble(Effort.nudged(1.23, up = true), 1.25, 1e-9)
    assertEqualsDouble(Effort.nudged(1.23, up = false), 1.20, 1e-9)

  test("nudging a factor down never reaches zero, which would make every figure zero for ever"):
    val floored = Iterator.iterate(1.0)(Effort.nudged(_, up = false)).drop(100).next()

    assert(floored > 0, s"a factor nudged down a hundred times reached $floored")

  test("every step of the grid moves in the direction it was asked to"):
    // A grid of hundredths against a step that is not exact in binary: 1.25 / 0.05 is 24.999999999999996, and a floor
    // taken on that would make "more" mean "no change" at exactly the value the app starts on.
    var value = 0.05
    while value < 3.0 do
      val up = Effort.nudged(value, up = true)
      val down = Effort.nudged(value, up = false)
      assert(up > value, f"$value%.2f did not go up: $up%.4f")
      assert(down < value || value <= 0.05, f"$value%.2f did not go down: $down%.4f")
      value = up

  test("a factor survives a round trip of nudges"):
    assertEqualsDouble(Effort.nudged(Effort.nudged(1.25, up = true), up = false), 1.25, 1e-9)

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
