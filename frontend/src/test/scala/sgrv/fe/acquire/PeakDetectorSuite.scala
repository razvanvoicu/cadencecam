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

    found.sliding(2).foreach:
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
