package sgrv.fe.bench

/** How fast the animation moves, and how that speed itself varies.
  *
  * A perfectly metronomic movement is the easy case, and the detector has already been shown to handle it. Real
  * exercise wanders, so the instantaneous frequency here wanders too: `hz` is the average, and it is modulated by a
  * slower oscillation of amplitude `swingHz` at `swingEveryHz`.
  */
private[fe] final case class Cadence(hz: Double, swingHz: Double = 0.2, swingEveryHz: Double = 0.05):
  require(hz > 0, "a cadence must be positive")
  require(swingHz >= 0 && swingHz < hz, "the swing must be smaller than the cadence it varies")

  /** The frequency at one instant, which is what the movement looks like rather than what it averages to. */
  def instantaneousHz(seconds: Double): Double = hz + swingHz * math.sin(2 * math.Pi * swingEveryHz * seconds)

  /** Cycles completed by a given moment, as a real number.
    *
    * The integral of the instantaneous frequency rather than `hz * t`: with a varying frequency those differ, and
    * taking the easy one would put the reference count out of step with what is actually on screen — which would then
    * be reported as the detector's error rather than the harness's.
    */
  def cyclesBy(seconds: Double): Double =
    // Floored at the start of the movement. Nothing before it has happened yet, and a negative elapsed time -- which
    // is what two different clocks being mixed looks like -- would otherwise come out as a large negative count
    // rather than as an obvious zero.
    val since = math.max(0.0, seconds)
    val swing =
      if swingEveryHz <= 0 then 0.0
      else swingHz * (1 - math.cos(2 * math.Pi * swingEveryHz * since)) / (2 * math.Pi * swingEveryHz)
    hz * since + swing

  def phaseAt(seconds: Double): Double = 2 * math.Pi * cyclesBy(seconds)

  /** Whole cycles finished by a given moment: the reference rep count. */
  def repsBy(seconds: Double): Int = math.floor(cyclesBy(seconds)).toInt

  /** When the nth cycle finishes, found by bisection.
    *
    * `cyclesBy` is monotonic because the swing is required to stay below the cadence, so a bisection converges and
    * there is no closed form to prefer. Used to say how far behind the counter is in seconds rather than in reps, which
    * is the unit a tolerance about lag belongs in.
    */
  def timeOfRep(n: Int): Double =
    if n <= 0 then 0.0
    else
      var low = 0.0
      var high = n / math.max(1e-9, hz - swingHz)
      var step = 0
      while step < 60 do
        val mid = (low + high) / 2
        if cyclesBy(mid) < n then low = mid else high = mid
        step += 1
      (low + high) / 2

/** Where the moving shape is at a given phase, in fractions of the drawing area.
  *
  * Coordinates run 0 to 1 with the origin at the top left, matching a canvas rather than the mathematical quadrants the
  * detector names. The quadrant centres below are stated in those terms so the trajectories can be described the way
  * the exercises they stand in for are.
  */
private[fe] object Trajectory:
  /** Centres of the four quadrants as the detector numbers them: Q2 Q1 across the top, Q3 Q4 across the bottom. */
  val Q1 = (0.75, 0.25)
  val Q2 = (0.25, 0.25)
  val Q3 = (0.25, 0.75)
  val Q4 = (0.75, 0.75)

  private def between(from: (Double, Double), to: (Double, Double), at: Double): (Double, Double) =
    (from._1 + (to._1 - from._1) * at, from._2 + (to._2 - from._2) * at)

  /** A disc whose centre travels a circle once per cycle, crossing all four quadrants. */
  def circular(phase: Double, radius: Double = 0.28): (Double, Double) =
    (0.5 + radius * math.cos(phase), 0.5 - radius * math.sin(phase))

  /** The far end of a bar pivoting about the centre of Q3, swinging between the centres of Q4 and Q2.
    *
    * A quarter arc rather than a straight line between the two. Q4 and Q2 are each the same distance from Q3, so
    * sweeping the angle keeps the bar the same length throughout -- which is what a limb does. Interpolating between
    * the endpoints instead would trace the chord, and the bar would shorten to seven tenths of its length at mid-swing
    * and grow back: a change in size where the exercise has none, and one the detector would see.
    *
    * A cycle is out and back, so the end is at Q4 at phase zero, at Q2 halfway, and back at Q4 by the end.
    */
  def swingingEnd(phase: Double): (Double, Double) =
    val sweep = (math.Pi / 2) * (1 - math.cos(phase)) / 2
    (Q3._1 + BarLength * math.cos(sweep), Q3._2 - BarLength * math.sin(sweep))

  /** The pivot-to-tip distance, which is exactly the span from the centre of Q3 to the centre of Q4. */
  val BarLength: Double = Q4._1 - Q3._1

  /** A shape shuttling from the centre of Q4 to the centre of Q1 and back, once per cycle. */
  def shuttle(phase: Double): (Double, Double) =
    between(Q4, Q1, (1 - math.cos(phase)) / 2)
