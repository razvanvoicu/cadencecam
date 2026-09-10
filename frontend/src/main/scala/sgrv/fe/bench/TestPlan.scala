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

  /** Only the two extremes for now. The greys, which are where contrast starts to matter, come once these have been
    * run and there is something to compare them against.
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
  val PauseSeconds = 20

  /** How far the counter may lag or lead before it is worth recording, in seconds of movement.
    *
    * In seconds rather than in reps because the same shortfall means different things at different cadences: one
    * rep behind at half a hertz is two seconds, and at two hertz it is half of one.
    */
  val ToleranceSeconds = 2.0

  val DefaultReps = 30
  val DefaultCadence: Cadence = Cadence(hz = 1.0, swingHz = 0.2, swingEveryHz = 0.05)

  /** The suite: every figure in every palette, at one cadence.
    *
    * Deliberately small. The point of the first run is to find out which combinations the detector struggles with,
    * and a suite large enough to be informative about everything would take longer to run than anyone will watch.
    */
  def standard(reps: Int = DefaultReps, cadence: Cadence = DefaultCadence): Seq[TestCase] =
    for
      palette <- Palette.All
      figure <- Figure.values.toSeq
    yield TestCase(figure, palette, cadence, reps)
