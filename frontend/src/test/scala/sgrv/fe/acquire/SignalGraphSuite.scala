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

  test("the widest step spans the whole brightness range"):
    assertEqualsDouble(SignalZoom.span(SignalZoom.Span128) * 2, 255.0, 1.0)

  test("every step is centred on the series, so a deviation series is not pushed off the pane"):
    // A common-mode-corrected channel hovers about zero and goes negative; an absolute 0-255 window would draw it
    // at or below the bottom edge. Centring is what keeps raw and corrected comparable in one pane.
    val corrected = Seq(-4.0, 2.0, -1.0, 3.0)

    SignalZoom.values.foreach: zoom =>
      val (low, high) = SignalGraph.range(corrected, zoom)
      val centre = (low + high) / 2
      assertEqualsDouble(centre, corrected.sum / corrected.size, 1e-9)
      assert(low < corrected.min || high > corrected.max || zoom == SignalZoom.Span2, s"$zoom must contain the data")

  test("a zoomed span is fixed in width and centred on the window mean"):
    val samples = Seq(100.0, 108.0, 104.0)
    val (low, high) = SignalGraph.range(samples, SignalZoom.Span32)

    assertEqualsDouble(high - low, 64.0, 1e-9)
    assertEqualsDouble((low + high) / 2, 104.0, 1e-9)

  test("the span never widens to fit the data, which is what keeps heights meaningful"):
    // A signal far larger than the span is clipped by the pane rather than rescaling it.
    val wild = Seq(0.0, 255.0, 128.0)
    val (low, high) = SignalGraph.range(wild, SignalZoom.Span8)

    assertEqualsDouble(high - low, 16.0, 1e-9)
    assert(low > 0.0 && high < 255.0, "the span must stay put rather than growing to contain the samples")

  test("the same change draws the same height wherever it sits on the scale"):
    def heightOf(samples: Seq[Double], delta: Double, zoom: SignalZoom): Double =
      val (low, high) = SignalGraph.range(samples, zoom)
      delta / (high - low)

    // A ten-unit step is the same fraction of the pane in a dark scene and a bright one.
    val dark = heightOf(Seq(40.0, 44.0), 10.0, SignalZoom.Span32)
    val bright = heightOf(Seq(200.0, 204.0), 10.0, SignalZoom.Span32)

    assertEqualsDouble(dark, bright, 1e-12)
    assertEqualsDouble(dark, 10.0 / 64.0, 1e-12)

  test("recentring follows drift without changing how big a step looks"):
    val before = SignalGraph.range(Seq(100.0), SignalZoom.Span32)
    val afterDrift = SignalGraph.range(Seq(140.0), SignalZoom.Span32)

    assertEqualsDouble(afterDrift._1 - before._1, 40.0, 1e-9)
    assertEqualsDouble(afterDrift._2 - afterDrift._1, before._2 - before._1, 1e-9)

  test("every zoom is reachable from every other, and the widest stays within the requested bound"):
    val cycle = Iterator.iterate(SignalZoom.Span128)(SignalZoom.next).take(5).toList

    assertEquals(cycle.take(4).toSet, SignalZoom.values.toSet)
    assertEquals(cycle.head, cycle.last, "cycling must return to where it started")
    // The step the investigation settled on keeps the trace within the agreed distance of its own mean.
    assertEqualsDouble(SignalZoom.span(SignalZoom.Span32), 32.0, 1e-9)
    assertEquals(SignalZoom.label(SignalZoom.Span32), "±32")
    assertEquals(SignalZoom.label(SignalZoom.Span128), "±128")

  test("an empty window still yields a drawable range at every zoom"):
    SignalZoom.values.foreach: zoom =>
      val (low, high) = SignalGraph.range(Seq.empty, zoom)
      assert(high > low, s"$zoom would divide by zero when scaling")
