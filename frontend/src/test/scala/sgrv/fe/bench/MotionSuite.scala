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

  test("the suite runs every figure in both themes"):
    val plan = TestPlan.standard()

    assertEquals(plan.size, 6)
    assertEquals(plan.map(_.figure).distinct.toSet, Figure.values.toSet)
    assertEquals(plan.count(_.palette.theme == Theme.Darker), 3, "three darker tests")
    assertEquals(plan.count(_.palette.theme == Theme.Lighter), 3, "three lighter tests")
    // Alternating, so the two runs of one figure sit next to each other and differ only in which way contrast points.
    plan.grouped(2).foreach(pair => assertEquals(pair.map(_.figure).distinct.size, 1))

  test("the two bands are the ones asked for, and do not meet"):
    // Not black on white. That was the easiest signal a camera can be given, and passing it said little; these bands
    // sit a quarter of the scale apart, which is nearer to what a room offers.
    assertEquals(Grey.Dark, 32 to 95)
    assertEquals(Grey.Light, 128 to 191)
    assert(Grey.Dark.end < Grey.Light.start, "the two bands must not overlap")

  test("a palette puts the lighter band where its theme says"):
    val darker = Palette.of(Theme.Darker)
    assertEquals(darker.ink, Grey.Light)
    assertEquals(darker.ground, Grey.Dark)
    assertEquals(darker.name, "grey 128-191 on 32-95")

    val lighter = Palette.of(Theme.Lighter)
    assertEquals(lighter.ink, Grey.Dark)
    assertEquals(lighter.ground, Grey.Light)

  test("a region holds every level of its band, exactly as often as every other"):
    // Not sixty-five thousand independent draws: that leaves the histogram visibly ragged, some levels a few percent
    // over and others under. Naming a band is a claim that the test covers it evenly, so the levels are dealt out and
    // then shuffled, which makes the claim exactly true rather than true on average over enough runs.
    for band <- Seq(Grey.Dark, Grey.Light) do
      val pixels = Texture.TileEdge * Texture.TileEdge
      val counts = Texture.levels(band, pixels, () => scala.util.Random.nextDouble()).groupBy(identity)

      assertEquals(counts.keySet, band.toSet, "every level of the band must appear")
      assertEquals(counts.values.map(_.length).toSet, Set(pixels / band.length), "and all of them equally often")

  test("a region is speckled, not flat"):
    // The mistake this replaces: one level drawn per test and painted flat across the whole region. A camera sees
    // texture, and a scene with none asks nothing of its denoising, its metering or its compression.
    val speckle = Texture.levels(Grey.Dark, 640, () => scala.util.Random.nextDouble())

    assert(speckle.distinct.length > 1, "a region of one level is the flat fill this exists to replace")
    assert(speckle.toSeq != speckle.sorted.toSeq, "dealt but never shuffled would band the region instead")

  test("a tile can be divided evenly by either band"):
    assertEquals(Texture.TileEdge * Texture.TileEdge % Grey.Dark.length, 0)
    assertEquals(Texture.TileEdge * Texture.TileEdge % Grey.Light.length, 0)
    intercept[IllegalArgumentException](Texture.levels(Grey.Dark, 100, () => 0.5))

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

  test("the buffer is long enough to hold the test a capture covers"):
    // One recording per test, asked for during the pause and before the reset that wipes the buffer, so what has to
    // fit is a single test and its pause rather than the whole run. It was the whole run when a suite filed one
    // capture at the end, and a suite of six would no longer come close to fitting.
    val recordedSeconds =
      sgrv.fe.acquire.QuadrantSignals.Recorded * sgrv.fe.acquire.FrameSampler.DefaultIntervalMillis / 1000.0
    val longest = TestPlan.standard().map(TestPlan.testSeconds).max

    assert(
      recordedSeconds > longest,
      f"a test runs $longest%.0fs but only ${recordedSeconds}%.0fs is kept"
    )

  test("a suite takes about a quarter of an hour, and the arithmetic says so plainly"):
    // Six tests rather than two, and worth stating outright: whoever starts a run should know they are committing to
    // it rather than discovering the length halfway through.
    assertEqualsDouble(TestPlan.durationSeconds(TestPlan.standard()), 868.5, 1.0)

  test("a capture is asked for before the reset that would wipe what it records"):
    // The reset now clears the signal buffer, so the order is load-bearing rather than incidental: capturing after
    // it would file an empty recording and lose the test that produced it.
    assert(
      Bench.CaptureBeforeResetMillis > 0,
      "there must be time between asking for a recording and wiping the buffer it comes from"
    )
    assert(
      Bench.CaptureBeforeResetMillis >= Bench.SettleAfterResetMillis,
      "sending a few hundred kilobytes from a phone deserves at least as long as a reset takes to land"
    )

  test("a per-test recording is well inside what the buffer holds"):
    // One trace per test now, each holding only its own movement and pause, so the six minutes kept is ample.
    val perTest = TestPlan.DefaultReps / TestPlan.DefaultCadence.hz + TestPlan.PauseSeconds
    val recorded =
      sgrv.fe.acquire.QuadrantSignals.Recorded * sgrv.fe.acquire.FrameSampler.DefaultIntervalMillis / 1000.0

    assert(recorded > perTest, f"a test runs ${perTest}%.0fs but only ${recorded}%.0fs is kept")

  test("the framing crosses sit exactly at the quadrant centres the figures reach"):
    // A mark that is nearly right is worse than none: it gets trusted, and the camera is lined up a little wrong
    // every run. On a square canvas the crosses must land on the same points the bar's tip and the square's stops do.
    val square = Painter.crossCentres(400, 400)

    assertEquals(square.toSet, Set((300.0, 100.0), (100.0, 100.0), (100.0, 300.0), (300.0, 300.0)))
    assertEquals(square.size, 4)

  test("the crosses follow the field when the canvas is not square"):
    // The trajectories live on a centred square field, so the crosses must too. Scaling them by width and height
    // independently would put them where the figure never goes -- the same mistake that once drew the circle as an
    // ellipse.
    val wide = Painter.crossCentres(600, 400)

    // A 400-wide field, centred, so its left edge is at 100.
    assertEquals(wide.toSet, Set((400.0, 100.0), (200.0, 100.0), (200.0, 300.0), (400.0, 300.0)))
    val tall = Painter.crossCentres(400, 600)
    assertEquals(tall.toSet, Set((300.0, 200.0), (100.0, 200.0), (100.0, 400.0), (300.0, 400.0)))

  test("the crosses are green, and small enough not to be the picture"):
    assertEquals(Painter.CrossColour, "rgb(0, 128, 0)")
    assert(Painter.CrossArm <= 0.01, "a cross this large would be a moving part of the scene, not a mark on it")
    // Between the bands, so nothing shown while framing hints at the theme of the test about to run.
    assert(Painter.IdleLevel > Grey.Dark.end && Painter.IdleLevel < Grey.Light.start)

  test("a result row names the two things that differ between tests"):
    // Everything else is the same in all six -- the bands, the cadence, the rep count -- so a row repeating them
    // would bury the part that says which test it was. A failure is read by what the failing rows share.
    val plan = TestPlan.standard()

    assertEquals(plan.map(_.shortName).distinct.size, plan.size, "two rows reading the same is a table of nothing")
    assertEquals(plan.head.shortName, "disc, darker")
    assertEquals(plan.last.shortName, "square, lighter")
