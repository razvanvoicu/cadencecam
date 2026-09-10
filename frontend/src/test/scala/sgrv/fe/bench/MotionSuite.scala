package sgrv.fe.bench

import munit.FunSuite

class MotionSuite extends FunSuite:

  test("a steady cadence completes exactly as many cycles as its frequency promises"):
    val steady = Cadence(hz = 1.0, swingHz = 0.0, swingEveryHz = 0.0)

    assertEqualsDouble(steady.cyclesBy(30.0), 30.0, 1e-9)
    assertEquals(steady.repsBy(29.999), 29)
    assertEquals(steady.repsBy(30.0), 30)

  test("cycles are the integral of the frequency, not the frequency times the time"):
    // With a varying frequency those differ, and using the easy one would put the reference count out of step with
    // what is on screen -- which would then be reported as the detector's error rather than the harness's.
    val varying = Cadence(hz = 1.0, swingHz = 0.2, swingEveryHz = 0.05)
    val naive = 1.0 * 7.0

    assert(math.abs(varying.cyclesBy(7.0) - naive) > 0.05, "the swing made no difference, so it is not integrated")

  test("the phase never runs backwards, whatever the swing"):
    val varying = Cadence(hz = 1.0, swingHz = 0.2, swingEveryHz = 0.05)
    val samples = (0 to 4000).map(i => varying.cyclesBy(i / 100.0))

    assert(samples.sliding(2).forall { case Seq(a, b) => b >= a }, "cycles went backwards")

  test("the instantaneous frequency swings by the amplitude asked for, about the average"):
    val varying = Cadence(hz = 1.0, swingHz = 0.2, swingEveryHz = 0.05)
    val over = (0 to 2000).map(i => varying.instantaneousHz(i / 10.0))

    assertEqualsDouble(over.max, 1.2, 0.01)
    assertEqualsDouble(over.min, 0.8, 0.01)

  test("a swing as large as the cadence is refused, since the movement would stop and reverse"):
    intercept[IllegalArgumentException](Cadence(hz = 1.0, swingHz = 1.0))
    intercept[IllegalArgumentException](Cadence(hz = 0.0))

  test("the time a rep finishes agrees with the count at that time"):
    val varying = Cadence(hz = 1.2, swingHz = 0.2, swingEveryHz = 0.05)

    (1 to 20).foreach: n =>
      val at = varying.timeOfRep(n)
      assertEquals(varying.repsBy(at + 1e-6), n, s"rep $n")
      assertEquals(varying.repsBy(at - 1e-3), n - 1, s"rep $n")

  test("the disc visits every quadrant once per cycle"):
    val quadrants = (0 until 360).map: degrees =>
      val (x, y) = Trajectory.circular(math.toRadians(degrees).toDouble)
      (x > 0.5, y < 0.5)

    assertEquals(quadrants.toSet.size, 4, "the circle did not cross all four quadrants")

  private def distanceFromPivot(phase: Double): Double =
    val (x, y) = Trajectory.swingingEnd(phase)
    math.hypot(x - Trajectory.Q3._1, y - Trajectory.Q3._2)

  test("the bar swings from Q4 to Q2 and back within one cycle"):
    val (outX, outY) = Trajectory.swingingEnd(0.0)
    assertEqualsDouble(outX, Trajectory.Q4._1, 1e-9)
    assertEqualsDouble(outY, Trajectory.Q4._2, 1e-9)
    val (upX, upY) = Trajectory.swingingEnd(math.Pi)
    assertEqualsDouble(upX, Trajectory.Q2._1, 1e-9)
    assertEqualsDouble(upY, Trajectory.Q2._2, 1e-9)
    val (backX, backY) = Trajectory.swingingEnd(2 * math.Pi)
    assertEqualsDouble(backX, Trajectory.Q4._1, 1e-9)
    assertEqualsDouble(backY, Trajectory.Q4._2, 1e-9)

  test("the bar keeps its length throughout the swing"):
    // It pivots, so its tip travels an arc. Interpolating between the two endpoints would trace the chord instead,
    // shortening the bar to about seven tenths at mid-swing and growing it back -- a change of size the exercise
    // does not have, and one the detector would see as signal.
    val lengths = (0 to 400).map(step => distanceFromPivot(2 * math.Pi * step / 400.0))

    assertEqualsDouble(lengths.min, Trajectory.BarLength, 1e-9)
    assertEqualsDouble(lengths.max, Trajectory.BarLength, 1e-9)

  test("the tip stays within the panel while it sweeps"):
    val points = (0 to 400).map(step => Trajectory.swingingEnd(2 * math.Pi * step / 400.0))

    assert(points.forall((x, y) => x >= 0.0 && x <= 1.0 && y >= 0.0 && y <= 1.0), "the bar swung off the panel")

  test("the square shuttles between Q4 and Q1"):
    assertEquals(Trajectory.shuttle(0.0), Trajectory.Q4)
    assertEquals(Trajectory.shuttle(math.Pi), Trajectory.Q1)

  test("the suite covers every figure in every palette"):
    val plan = TestPlan.standard()

    assertEquals(plan.size, Figure.values.length * Palette.All.size)
    assertEquals(plan.map(_.figure).toSet, Figure.values.toSet)
    assertEquals(plan.map(_.palette).toSet, Palette.All.toSet)

  test("time before the movement began counts as nothing, not as a large negative number"):
    // Two clocks were mixed once -- an epoch timestamp against the animation frame's milliseconds-since-load -- and
    // the elapsed time came out at minus fifty years, which overflowed into a count of -1789054616 on screen.
    val steady = Cadence(hz = 1.0, swingHz = 0.0, swingEveryHz = 0.0)

    assertEquals(steady.repsBy(-1.7e12), 0)
    assertEquals(steady.repsBy(-1.0), 0)
    assertEqualsDouble(steady.cyclesBy(-5.0), 0.0, 1e-9)
    assert(steady.repsBy(-1.7e12) >= 0, "a count must never be negative")

  test("the movement stays inside the band the detector passes, at every point of its swing"):
    // The harness must test the counter, not the filter. If the cadence dipped below the band-pass's low corner the
    // movement would be attenuated on the way in, and a miss would say nothing about counting.
    val detector = sgrv.fe.acquire.DetectorSettings()
    val cadence = TestPlan.DefaultCadence
    val extremes = (0 to 2000).map(step => cadence.instantaneousHz(step / 10.0))

    assert(extremes.min > detector.lowHz, s"the movement slows to ${extremes.min}Hz, below ${detector.lowHz}Hz")
    assert(extremes.max < detector.highHz, s"the movement reaches ${extremes.max}Hz, above ${detector.highHz}Hz")

  test("a test is long enough for something intermittent to show itself"):
    // Half the tests failed at thirty reps, and a different half each run: that is intermittency, and it needs room
    // to happen more than once within a single test.
    assert(TestPlan.DefaultReps >= 100, s"${TestPlan.DefaultReps} reps is too short to catch a stall and a recovery")

  test("a suite is long, and the arithmetic says so plainly"):
    val perTest = TestPlan.DefaultReps / TestPlan.DefaultCadence.hz + TestPlan.PauseSeconds
    val whole = perTest * TestPlan.standard().size

    assertEqualsDouble(perTest, 145.0, 1.0)
    assert(whole > 800, "a suite that short would not be the one described")
