package sgrv.fe.acquire

/** A second-order IIR section, applied as a difference equation one sample at a time.
  *
  * Coefficients follow Robert Bristow-Johnson's Audio EQ Cookbook — the same formulae the Web Audio API's own
  * BiquadFilterNode is built on. One section is enough here: the 0.5-2 Hz band is wide relative to its centre
  * (Q about 0.67), so nothing steeper is called for, and at 10 Hz the Nyquist limit of 5 Hz leaves ample margin.
  */
private[fe] final case class Biquad(b0: Double, b1: Double, b2: Double, a1: Double, a2: Double):

  /** Runs the filter over a series from rest.
    *
    * Filtering the whole buffer each time rather than carrying state between calls keeps the result a pure function
    * of the samples, so what is drawn and what is counted cannot drift apart from each other. It costs a few
    * thousand multiply-adds per second, which is nothing next to the frame capture that produced the samples.
    */
  def filter(samples: Seq[Double]): Seq[Double] =
    var x1, x2, y1, y2 = 0.0
    samples.map: x0 =>
      val y0 = b0 * x0 + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2
      x2 = x1
      x1 = x0
      y2 = y1
      y1 = y0
      y0

private[fe] object Biquad:
  /** A constant-peak-gain band-pass between two edge frequencies.
    *
    * The centre is their geometric mean, which is what makes the response symmetric on a log frequency axis, and Q
    * follows from the width: a 0.5-2 Hz band centres on 1 Hz with Q = 1/1.5.
    */
  def bandPass(lowHz: Double, highHz: Double, sampleRateHz: Double): Biquad =
    require(0 < lowHz && lowHz < highHz, "the band must have a positive, increasing range")
    require(highHz < sampleRateHz / 2, "the band must stay below the Nyquist frequency")
    val centreHz = math.sqrt(lowHz * highHz)
    val q = centreHz / (highHz - lowHz)
    val w0 = 2 * math.Pi * centreHz / sampleRateHz
    val alpha = math.sin(w0) / (2 * q)
    val a0 = 1 + alpha
    Biquad(
      b0 = alpha / a0,
      b1 = 0.0,
      b2 = -alpha / a0,
      a1 = -2 * math.cos(w0) / a0,
      a2 = (1 - alpha) / a0
    )
