package sgrv.fe.acquire

import munit.FunSuite

class BiquadSuite extends FunSuite:

  private val rate = 10.0
  private val band = Biquad.bandPass(0.5, 2.0, rate)

  /** A steady sinusoid, long enough for the filter's start-up transient to have died away. */
  private def tone(hz: Double, seconds: Double = 60.0, amplitude: Double = 1.0): Seq[Double] =
    Seq.tabulate((seconds * rate).toInt)(index => amplitude * math.sin(2 * math.Pi * hz * index / rate))

  private def settledAmplitude(samples: Seq[Double]): Double =
    val settled = samples.drop(samples.length / 2)
    settled.max - settled.min

  /** Output amplitude against the input's own, since at ten samples a cycle the discrete samples never land on the
    * true crest of a sinusoid and both are understated by the same factor.
    */
  private def gainAt(hz: Double): Double =
    val input = tone(hz)
    settledAmplitude(band.filter(input)) / settledAmplitude(input)

  test("passes the middle of the band nearly untouched"):
    // 1 Hz is the geometric centre of 0.5-2, where a constant-peak-gain band-pass has unity gain.
    assertEqualsDouble(gainAt(1.0), 1.0, 0.05)

  test("passes the whole target band at usable strength"):
    Seq(0.5, 0.75, 1.0, 1.5, 2.0).foreach: hz =>
      assert(gainAt(hz) > 0.4, f"$hz%.2f Hz was attenuated to ${gainAt(hz)}%.2f, which is inside the band")

  test("rejects the slow drift a band-pass exists to remove"):
    // Lighting drift and exposure wander live well below the band.
    val drift = settledAmplitude(band.filter(tone(0.05)))
    val inBand = settledAmplitude(band.filter(tone(1.0)))

    assert(drift < inBand / 10, f"drift came through at $drift%.3f against $inBand%.3f in band")

  test("removes a constant offset entirely"):
    // A band-pass has no response at DC, so a still scene's absolute brightness must not reach the detector.
    val settled = band.filter(Seq.fill(200)(137.0)).drop(100)

    settled.foreach(value => assertEqualsDouble(value, 0.0, 1e-6))

  test("rejects frequencies above the band"):
    val fast = settledAmplitude(band.filter(tone(4.5)))

    assert(fast < 0.4, f"4.5 Hz came through at $fast%.3f")

  test("rings for about a cycle when hit with a step, which is why steps must not reach it"):
    // The reason common-mode removal happens before filtering: a step is broadband, and what comes out the far
    // side looks like a rep.
    val step = Seq.fill(30)(0.0) ++ Seq.fill(70)(20.0)
    val response = band.filter(step)
    val peaks = PeakDetector.peaks(response, minimumDistance = 5, minimumProminence = 0.5)

    assert(peaks.nonEmpty, "a step should indeed ring, which is the hazard being guarded against")

  test("refuses a band it cannot represent at the sample rate"):
    intercept[IllegalArgumentException](Biquad.bandPass(0.5, 6.0, rate))
    intercept[IllegalArgumentException](Biquad.bandPass(2.0, 0.5, rate))
