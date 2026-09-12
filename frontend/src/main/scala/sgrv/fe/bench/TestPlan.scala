package sgrv.fe.bench

/** What the moving shape is, which decides both its path and how it is drawn. */
private[fe] enum Figure:
  /** A disc whose centre travels a circle, crossing every quadrant once per cycle. */
  case Disc

  /** A bar pivoting about the centre of Q3, its far end swinging between Q4 and Q2 -- the shape of a curl. */
  case Bar

  /** A square shuttling between the centres of Q4 and Q1. */
  case Square

/** Which way round the contrast runs: a light object on a dark ground, or a dark object on a light one. */
private[fe] enum Theme:
  case Darker
  case Lighter

/** The two bands the greys are drawn from.
  *
  * Black on white was never the thing being tested. It is the easiest signal a camera can be given, and every test run
  * in it said more about the screen than about the detector; the case that matters is a person in a room, where the
  * brightest thing in frame is rarely paper-white and the darkest is rarely ink. These two bands sit a quarter of the
  * scale apart rather than the whole of it, so a test asks whether the movement can be found at ordinary contrast.
  *
  * Sixty-four levels each, drawn uniformly, so no single pairing can be tuned for and a suite that passes has passed
  * across the band rather than at one convenient point in it.
  */
private[fe] object Grey:
  val Dark: Range = 32 to 95
  val Light: Range = 128 to 191

  /** As CSS, which wants each channel twice over: a grey has all three the same. */
  def css(level: Int): String = f"#$level%02x$level%02x$level%02x"

/** A foreground band against a background band.
  *
  * Bands rather than levels. Each region is speckled with every grey its band holds, equally often, so the moving
  * object and the ground behind it each have texture of their own -- which is what a camera is actually ever pointed
  * at. Two flat tones would be the easiest scene there is, and a detector that only ever passed that would have been
  * told nothing about a room.
  *
  * Contrast is the one thing counting has been observed to depend on, so it is a property of a test rather than a
  * detail of the page.
  */
private[fe] final case class Palette(ink: Range, ground: Range):
  def name: String = s"grey ${ink.start}-${ink.end} on ${ground.start}-${ground.end}"
  def theme: Theme = if ink.start > ground.start then Theme.Darker else Theme.Lighter

private[fe] object Palette:
  /** Which band goes where, which is all a theme is. */
  def of(theme: Theme): Palette = theme match
    case Theme.Darker  => Palette(ink = Grey.Light, ground = Grey.Dark)
    case Theme.Lighter => Palette(ink = Grey.Dark, ground = Grey.Light)

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

  /** Every figure in both themes: three darker tests and three lighter ones.
    *
    * Back to all six after a spell running only two. The pair was chosen when black on white was failing and white on
    * black was not, to put the two side by side in one trace; with the contrast now drawn from ordinary greys there is
    * no known-good and known-bad pairing to narrow down to, and the question is which figures survive which theme.
    *
    * Alternating rather than grouped, so the two runs of the same figure sit next to each other and differ only in
    * which way the contrast points.
    */
  def standard(reps: Int = DefaultReps, cadence: Cadence = DefaultCadence): Seq[TestCase] =
    for
      figure <- Figure.values.toSeq
      theme <- Seq(Theme.Darker, Theme.Lighter)
    yield TestCase(figure, Palette.of(theme), cadence, reps)

  /** How long one test runs: its movement and the pause that follows, which is what a single capture has to cover. */
  def testSeconds(test: TestCase): Double = test.reps / test.cadence.hz + PauseSeconds

  /** How long a whole suite runs, including the pause after each test and the settle between them. */
  def durationSeconds(plan: Seq[TestCase]): Double =
    plan.map(testSeconds).sum + (plan.size - 1) * Bench.SettleAfterResetMillis / 1000.0
