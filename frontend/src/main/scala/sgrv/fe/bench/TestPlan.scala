package sgrv.fe.bench

/** What the moving shape is, which decides both its path and how it is drawn. */
private[fe] enum Figure:
  /** A disc whose centre travels a circle, crossing every quadrant once per cycle. */
  case Disc

  /** A bar pivoting about the centre of Q3, its far end swinging between Q4 and Q2 -- the shape of a curl. */
  case Bar

  /** A square shuttling between the centres of Q4 and Q1. */
  case Square

/** A foreground against a background.
  *
  * Contrast is the one thing counting has been observed to depend on, so it is a property of a test rather than a
  * detail of the page: a suite that only ever ran white on black would say nothing about the case that fails.
  */
private[fe] final case class Palette(name: String, ink: String, ground: String)

private[fe] object Palette:
  val WhiteOnBlack = Palette("white on black", "#ffffff", "#000000")
  val BlackOnWhite = Palette("black on white", "#000000", "#ffffff")

  /** Only the two extremes for now. The greys, which are where contrast starts to matter, come once these have been run
    * and there is something to compare them against.
    */
  val All = Seq(WhiteOnBlack, BlackOnWhite)

/** One test: a figure moving at a cadence, in a palette, for a fixed number of reps. */
private[fe] final case class TestCase(figure: Figure, palette: Palette, cadence: Cadence, reps: Int):
  def name: String = f"${figure.toString.toLowerCase}, ${palette.name}, ${cadence.hz}%.2fHz x $reps"

private[fe] object TestPlan:
  /** How long to keep listening after the movement stops.
    *
    * The detector confirms a peak from samples that follow it, and reports over a socket, so the last rep of a set
    * arrives after the set has ended. Ending a test the moment the animation stops would score that as a miss.
    */
  val PauseSeconds = 30

  /** How far the counter may lag or lead before it is worth recording, in seconds of movement.
    *
    * In seconds rather than in reps because the same shortfall means different things at different cadences: one rep
    * behind at half a hertz is two seconds, and at two hertz it is half of one.
    */
  val ToleranceSeconds = 2.0

  /** Long enough for a stall to happen, be noticed, and recover inside a single test.
    *
    * Thirty reps often ended before the interesting part: about half of them failed, and which half varied between
    * runs, which is the signature of something intermittent rather than of a figure the detector cannot see. A hundred
    * gives the intermittent thing room to show itself more than once per test.
    */
  val DefaultReps = 100

  /** A little slower than before, and still comfortably inside the band the detector passes.
    *
    * The swing takes the instantaneous frequency down to 0.6Hz, against a low corner of 0.5Hz -- close enough that
    * lowering the average further would start attenuating the movement rather than testing the counter.
    */
  val DefaultCadence: Cadence = Cadence(hz = 0.8, swingHz = 0.2, swingEveryHz = 0.05)

  /** Two tests: one expected to pass, one expected to fail.
    *
    * Running every figure in every palette produced failures in about half of them, differing between runs, which says
    * the trouble is intermittent but not where it lives. Two cases chosen for contrast are worth more: a disc on a
    * circle in white on black is the arrangement that has counted reliably, and a bar crunching in black on white is
    * the one predicted to break -- either losing the cadence or counting something quite different.
    *
    * Together in one suite they land in a single captured trace, so the signal that worked and the signal that did not
    * can be read side by side out of the same recording, under the same lighting and the same framing. That is what
    * makes it material for a realistic unit test rather than another anecdote.
    */
  def standard(reps: Int = DefaultReps, cadence: Cadence = DefaultCadence): Seq[TestCase] =
    Seq(
      TestCase(Figure.Disc, Palette.WhiteOnBlack, cadence, reps),
      TestCase(Figure.Bar, Palette.BlackOnWhite, cadence, reps)
    )

  /** How long a whole suite runs, including the pause after each test and the settle between them. */
  def durationSeconds(plan: Seq[TestCase] = standard()): Double =
    plan.map(test => test.reps / test.cadence.hz + PauseSeconds).sum +
      (plan.size - 1) * Bench.SettleAfterResetMillis / 1000.0
