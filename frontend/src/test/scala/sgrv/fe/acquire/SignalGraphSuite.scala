package sgrv.fe.acquire

import munit.FunSuite

class SignalGraphSuite extends FunSuite:

  test("the window spans about five seconds at the sampling rate"):
    assertEquals(SignalGraph.WindowSamples * FrameSampler.DefaultIntervalMillis, 5000)

  test("oldest sits at the left edge and newest at the right"):
    assertEqualsDouble(SignalGraph.positionOf(0), 0.0, 0.0001)
    assertEqualsDouble(SignalGraph.positionOf(SignalGraph.WindowSamples - 1), 1.0, 0.0001)

  test("a partly filled window grows rightwards instead of stretching to fill"):
    // Ten samples into a fifty-wide window occupy the left fifth, so the trace scrolls rather than rescaling.
    val newest = SignalGraph.positionOf(9)

    assert(newest > 0.0 && newest < 0.25, s"expected the trace to occupy the left of the pane, got $newest")

  test("a movement larger than the floor fills the height available to it"):
    val (low, high) = SignalGraph.range(Seq(110.0, 140.0, 125.0))

    assertEqualsDouble(low, 110.0, 1e-9)
    assertEqualsDouble(high, 140.0, 1e-9)

  test("a scene at rest draws very nearly flat rather than magnified noise"):
    // Sensor noise of a unit or two must not be stretched to full height just because nothing else is happening.
    val atRest = Seq(120.0, 120.6, 119.7, 120.2, 119.9)
    val (low, high) = SignalGraph.range(atRest)

    assertEqualsDouble(high - low, SignalGraph.MinimumRange, 1e-9)
    // The noise occupies well under a tenth of the pane.
    assert((atRest.max - atRest.min) / (high - low) < 0.1, "resting noise should barely register")
    assert(low < atRest.min && high > atRest.max, "the floor must still contain the samples")

  test("the floor is a sixteenth of the brightness scale"):
    assertEqualsDouble(SignalGraph.MinimumRange, 16.0, 1e-9)
    assert(SignalGraph.MinimumRange < 256.0 / 8, "a floor this wide would flatten real movement too")

  test("heights are not comparable between panes above the floor, which is the cost of filling the space"):
    val modest = SignalGraph.range(Seq(100.0, 130.0))
    val large = SignalGraph.range(Seq(60.0, 200.0))

    assert(modest._2 - modest._1 < large._2 - large._1, "a quieter window should scale to a narrower range")

  test("a window with no variation at all still yields a drawable range"):
    val (low, high) = SignalGraph.range(Seq.fill(10)(120.0))

    assertEqualsDouble(high - low, SignalGraph.MinimumRange, 1e-9)

  test("an empty window still yields a drawable range"):
    val (low, high) = SignalGraph.range(Seq.empty)

    assert(high > low)
