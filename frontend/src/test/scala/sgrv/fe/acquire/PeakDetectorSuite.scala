package sgrv.fe.acquire

import munit.FunSuite

class PeakDetectorSuite extends FunSuite:

  test("finds one peak per cycle of a clean signal"):
    // Ten seconds at 1 Hz is ten reps.
    val wave = Seq.tabulate(100)(index => math.sin(2 * math.Pi * index / 10.0))
    val found = PeakDetector.peaks(wave, minimumDistance = 5, minimumProminence = 0.5)

    assertEquals(found.size, 10)

  test("measures prominence against the higher flanking valley"):
    // The right valley is shallower, so it, not the deep left one, sets the prominence.
    val samples = Seq(0.0, 1.0, 5.0, 3.0, 4.0)

    assertEqualsDouble(PeakDetector.prominence(samples, 2), 2.0, 1e-9)

  test("rejects a bump riding on the flank of a real cycle"):
    // The kind of secondary maximum a non-sinusoidal rep produces: a local peak that never descends far.
    val samples = Seq(0.0, 2.0, 10.0, 9.5, 9.8, 4.0, 0.0)
    val all = PeakDetector.localMaxima(samples)
    val accepted = PeakDetector.peaks(samples, minimumDistance = 1, minimumProminence = 1.0)

    assertEquals(all.size, 2, "both maxima are there to be found")
    assertEquals(accepted, Seq(2), "only the prominent one is a rep")

  test("keeps the more prominent of two peaks that are too close together"):
    val samples = Seq(0.0, 4.0, 0.0, 9.0, 0.0)
    val accepted = PeakDetector.peaks(samples, minimumDistance = 3, minimumProminence = 1.0)

    assertEquals(accepted, Seq(3))

  test("a cadence faster than the minimum distance is not double counted"):
    // Two hertz is the ceiling; anything closer than five samples at 10 Hz is not a separate rep.
    val fast = Seq.tabulate(100)(index => math.sin(2 * math.Pi * index / 3.0))
    val found = PeakDetector.peaks(fast, minimumDistance = 5, minimumProminence = 0.5)

    found
      .sliding(2)
      .foreach:
        case Seq(first, second) => assert(second - first >= 5, s"peaks at $first and $second are too close")
        case _                  => ()

  test("a flat signal has no peaks at all"):
    assertEquals(PeakDetector.peaks(Seq.fill(50)(3.0), 5, 0.1), Seq.empty)

  test("the newest sample is never a peak, since the turn back down confirms it"):
    val rising = Seq(0.0, 1.0, 2.0, 3.0, 9.0)

    assertEquals(PeakDetector.peaks(rising, 1, 0.5), Seq.empty)

  test("too short a series yields nothing rather than failing"):
    assertEquals(PeakDetector.peaks(Seq(1.0, 2.0), 1, 0.1), Seq.empty)
    assertEquals(PeakDetector.peaks(Seq.empty, 1, 0.1), Seq.empty)

  /** Q4 of a real recording, from the last nine seconds of a bench test: a dark disc circling on a light ground, the
    * arrangement that ended every such test one rep over. Sample 1160 onwards of "bench 1789178722823-490, test 2:
    * disc, grey 32-95 on 128-191, 0.80Hz x 100".
    *
    * The quadrant sits near 218 with the disc away and drops to about 191 as it passes through. The last of those
    * passes is at index 52; at 62 the brightness rises to the resting level once more and simply stays there, because
    * that is the disc leaving the frame at the end of the set rather than crossing the quadrant again.
    */
  private val discLeavingTheFrame: Seq[Double] = Seq(
    217.56, 199.34, 191.14, 191.25, 202.23, 220.55, 219.97, 219.43, 218.90, 218.43, 217.92, 217.64, 217.53, 217.66,
    202.77, 191.17, 191.48, 207.05, 220.23, 219.88, 219.04, 218.61, 218.19, 217.87, 217.64, 217.62, 195.55, 190.75,
    197.70, 220.33, 219.89, 219.26, 218.82, 218.37, 218.05, 217.74, 213.36, 193.33, 190.87, 205.93, 220.22, 219.63,
    219.01, 218.37, 218.15, 217.84, 217.74, 196.28, 190.66, 200.69, 220.13, 219.69, 218.92, 218.47, 218.07, 217.83,
    216.62, 194.76, 190.94, 207.31, 219.73, 218.58, 218.14, 217.64, 217.42, 217.16, 216.84, 216.71, 216.60, 216.54,
    216.47, 216.48, 216.48, 216.49, 216.42, 216.47, 216.39, 216.40, 216.33, 216.28, 216.33, 216.32, 216.32, 216.33
  )

  test("a peak the movement never came back from is not a rep"):
    // The phantom this exists for. Counted, it made every dark-on-light test read 101 where 100 reps were performed;
    // to the band-pass it is indistinguishable from the real peaks, since a filter blind to a constant cannot tell a
    // level that rose and stayed from one that rose and fell.
    val crossings = Seq(31, 42, 52)
    val departure = 62

    val kept = PeakDetector.returning(
      discLeavingTheFrame,
      crossings :+ departure,
      DetectorSettings().maximumGapSamples,
      DetectorSettings().returnFraction
    )

    assertEquals(kept, crossings, "the departure was counted as a rep, or a real crossing was thrown away")

  test("how far each of those peaks came back, which is what separates them"):
    val within = DetectorSettings().maximumGapSamples
    val falls = Seq(31, 42, 52, 62).map(PeakDetector.fallAfter(discLeavingTheFrame, _, within).get)

    // Three round trips of about the same depth, then one that goes nowhere: a factor of ten between them.
    assert(falls.take(3).forall(_ > 20.0), s"the real crossings should fall right back, got ${falls.take(3)}")
    assert(falls.last < 3.0, s"the departure should not fall at all, got ${falls.last}")

  test("a peak too near the end of the buffer is held back rather than judged"):
    // It has not been shown to be a rep yet, and the next samples will say. Judging it now would either invent a rep
    // at the end of every window or throw one away.
    assertEquals(PeakDetector.fallAfter(discLeavingTheFrame, 80, 20), None)
    assert(PeakDetector.fallAfter(discLeavingTheFrame, 52, 20).isDefined)
