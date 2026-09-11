package sgrv.fe.acquire

import munit.FunSuite

class SignalStrengthSuite extends FunSuite:

  test("the bands are set where counting was measured to degrade"):
    // Real figures from four phones running a movement of known cadence. Every disc reading counted every rep;
    // every bar reading lost reps and was slow to find the cadence.
    val discs = Seq(4.41, 3.39, 4.70, 4.17)
    val bars = Seq(1.42, 1.76, 1.96, 1.54)

    assert(discs.forall(SignalStrength.of(_) == SignalStrength.Strong), "a movement that counted must read strong")
    assert(bars.forall(SignalStrength.of(_) != SignalStrength.Strong), "a movement that lost reps must not read strong")

  test("the boundaries belong to the better band"):
    assertEquals(SignalStrength.of(SignalStrength.StrongAbove), SignalStrength.Strong)
    assertEquals(SignalStrength.of(SignalStrength.AdequateAbove), SignalStrength.Adequate)
    assertEquals(SignalStrength.of(SignalStrength.AdequateAbove - 0.01), SignalStrength.Weak)

  test("a movement only just above the background is weak, not merely adequate"):
    // One is the threshold itself: a peak that only just cleared the bar it had to clear.
    assertEquals(SignalStrength.of(1.0), SignalStrength.Weak)
    assertEquals(SignalStrength.of(0.0), SignalStrength.Weak)

  test("the label carries the number, so colour is not the only signal"):
    assertEquals(SignalStrength.label(3.24), "3.2×")
    assertEquals(SignalStrength.label(1.0), "1.0×")

  test("a channel reports how far its peaks stood above the bar they had to clear"):
    val settings = DetectorSettings()
    // A clean cadence well above the floor.
    val strong = Seq.tabulate(400)(i => 120.0 + 12.0 * math.sin(2 * math.Pi * i / 12.5))
    val faint = Seq.tabulate(400)(i => 120.0 + 2.0 * math.sin(2 * math.Pi * i / 12.5))

    val loud = RepAnalysis.analyse(strong, Quadrant.Q4, settings)
    val quiet = RepAnalysis.analyse(faint, Quadrant.Q4, settings)

    assert(loud.margin.exists(_ > 1.0), s"a strong movement should stand clear, got ${loud.margin}")
    // Too faint to clear the floor at all: no peaks, so nothing to report rather than a made-up number.
    assertEquals(quiet.margin.flatMap(m => Option.when(quiet.peaks.isEmpty)(m)), None)

  test("a still scene reports no margin at all, rather than a small one"):
    val still = Seq.fill(400)(120.0)

    assertEquals(RepAnalysis.analyse(still, Quadrant.Q1, DetectorSettings()).margin, None)

  test("the reading carries the margin so the view need not recompute it"):
    val counter = RepCounter()
    val channels = Quadrant.All.map(_ -> Seq.tabulate(400)(i => 120.0 + 12.0 * math.sin(2 * math.Pi * i / 12.5))).toMap
    for taken <- 1 to 400 do counter.update(channels.view.mapValues(_.take(taken)).toMap, taken)

    assert(counter.reading.margin.isDefined, "a moving scene must report a margin")

  test("the margin is measured against the floor, not against the scene's own activity"):
    // Dividing by the adaptive threshold flatters a quiet scene, because the threshold falls with it. Measured
    // against real recordings that inverted the answer: a camera shaken by typing scored higher than a movement
    // that counted every rep.
    val settings = DetectorSettings()
    val loud = Seq.tabulate(400)(i => 120.0 + 12.0 * math.sin(2 * math.Pi * i / 12.5))

    val analysis = RepAnalysis.analyse(loud, Quadrant.Q4, settings)
    val proms = analysis.peaks.map(i => PeakDetector.prominence(analysis.filtered.drop(settings.settlingSamples),
      i - settings.settlingSamples)).sorted
    val expected = proms(proms.size / 2) / settings.prominenceFloor

    assertEqualsDouble(analysis.margin.get, expected, 0.01)

