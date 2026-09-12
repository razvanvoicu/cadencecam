package sgrv.fe.bench

import org.scalajs.dom
import scala.scalajs.js

/** Where a suite has got to.
  *
  * `Pausing` is not idleness. The detector confirms a rep from samples that follow it and reports over a socket, so the
  * last reps of a set arrive after the movement has stopped; a test scored the instant the animation ends would mark
  * those as missing. The pause is part of the measurement.
  */
private[fe] enum Stage:
  case Idle
  case Running(index: Int, startedAt: Double)
  case Pausing(index: Int, until: Double, reference: Int)

  /** Between one test being scored and the next beginning.
    *
    * A state of its own rather than a gap. Scoring happens on the frame that finds the pause over, and the next test
    * does not begin for another second and a half; without somewhere else to be, every frame in between would find the
    * pause over again and score the same test repeatedly -- which it did, ninety times, along with ninety resets sent
    * to the other device.
    */
  case Settling(justFinished: Int)
  case Finished

private[fe] object Stage:
  /** The stage a frame at `now` leaves behind, and the test this frame scores, if any.
    *
    * Pure, and separated from the view, because this is where both of the bench's bugs have been. Scoring only
    * schedules the next test -- a second and a half later, so the reset can reach the other device -- and a stage that
    * stayed on `Pausing` in the meantime would find the pause over on every frame and score the same test again, sixty
    * times a second. Leaving immediately is the whole point, and it is asserted rather than assumed.
    */
  def onFrame(stage: Stage, now: Double): (Stage, Option[(Int, Int)]) = stage match
    case Pausing(index, until, expected) if now >= until => (Settling(index), Some(index -> expected))
    case other                                           => (other, None)

  /** The background to hold once the movement is over, with the figure gone.
    *
    * A set ends with the weight being put down, so the object leaves the frame -- and that is what gives the last rep
    * the trough after it that every other rep had. Holding the figure on screen instead left that peak one-sided, worth
    * half the prominence of its neighbours, and it went uncounted until the threshold decayed enough to admit it:
    * twenty seconds on one test and thirty on another, measured from real recordings.
    *
    * `None` while the movement is running, when the figure belongs on screen, and before a suite has begun.
    */
  def restingPalette(stage: Stage, plan: Seq[TestCase]): Option[Palette] = stage match
    case Pausing(index, _, _) => plan.lift(index).map(_.palette)
    case Settling(index)      => plan.lift(index).map(_.palette)
    case _                    => None

private[fe] final case class Outcome(test: String, reference: Int, acquired: Int, passed: Boolean)

/** Draws the moving figure of a test onto a canvas.
  *
  * Everything is in fractions of the canvas, so what the camera sees depends on how the screen is framed rather than on
  * the pixel size of any element -- and the quadrants the trajectories are described in are the quadrants the detector
  * will divide its own view into, provided the camera frames this canvas and not the whole page.
  */
private[fe] object Painter:

  /** The background alone, with the moving figure gone.
    *
    * What a set actually ends with. The exerciser puts the weight down before reaching for the phone, and the object
    * leaves the frame -- which gives the last rep's peak the trough after it that every other peak has. Holding the
    * figure on screen instead left that final peak one-sided, worth half the prominence of its neighbours, and it went
    * uncounted until the threshold decayed enough to admit it: twenty seconds on one test and thirty on another,
    * measured.
    */
  def clear(canvas: dom.HTMLCanvasElement, palette: Palette): Unit =
    val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    context.fillStyle = Texture.paint(context, palette.ground)
    context.fillRect(0, 0, canvas.width.toDouble, canvas.height.toDouble)
    crosses(context, canvas.width.toDouble, canvas.height.toDouble)

  /** Mid grey and the crosses, for aiming the camera before a suite begins.
    *
    * Deliberately between the two bands a test draws from, so nothing on screen while framing suggests the theme of the
    * test that will follow.
    */
  def idle(canvas: dom.HTMLCanvasElement): Unit =
    val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    context.fillStyle = Grey.css(IdleLevel)
    context.fillRect(0, 0, canvas.width.toDouble, canvas.height.toDouble)
    crosses(context, canvas.width.toDouble, canvas.height.toDouble)

  /** The grey shown before a suite starts: between the dark band and the light one, belonging to neither. */
  val IdleLevel: Int = (Grey.Dark.end + Grey.Light.start) / 2

  /** The colour of the framing crosses. Green because nothing else on this canvas is ever coloured, so they cannot be
    * mistaken for part of a test -- and a mid green rather than a bright one, so they are easy to see without being the
    * brightest thing in frame and skewing the exposure the camera settles on.
    */
  val CrossColour = "rgb(0, 128, 0)"

  /** A small cross at the centre of each quadrant, to aim the camera by.
    *
    * Framing is done by eye and has been the least reliable part of running a suite: the trajectories are described in
    * quadrants, and they only mean anything if the camera's quadrants are the canvas's. Four marks at the exact centres
    * make that alignment something to check rather than to judge.
    *
    * Static, so they contribute nothing to a band-passed signal; small, so the moving figure passing over one -- which
    * the bar and the square both do, their endpoints being those very centres -- changes little.
    */
  private def crosses(context: dom.CanvasRenderingContext2D, width: Double, height: Double): Unit =
    val arm = math.min(width, height) * CrossArm
    context.strokeStyle = CrossColour
    context.lineWidth = math.max(1.0, math.min(width, height) * 0.008)
    context.lineCap = "butt"
    for (centreX, centreY) <- crossCentres(width, height) do
      context.beginPath()
      context.moveTo(centreX - arm, centreY)
      context.lineTo(centreX + arm, centreY)
      context.moveTo(centreX, centreY - arm)
      context.lineTo(centreX, centreY + arm)
      context.stroke()

  /** Half the length of a cross's arms, as a fraction of the field: small enough that the figure passing over one
    * changes little, large enough to pick out through a camera across a room.
    */
  val CrossArm = 0.03

  /** Where the four crosses go, in canvas pixels.
    *
    * Separated out because a mark that is not exactly at a quadrant's centre is worse than no mark: it would be
    * trusted, and the camera would be lined up a little wrong every time. Same centred square field the trajectories
    * use, so a cross sits where the figure's own endpoints do -- which is what makes the alignment checkable by eye.
    */
  private[bench] def crossCentres(width: Double, height: Double): Seq[(Double, Double)] =
    val field = math.min(width, height)
    val left = (width - field) / 2
    val top = (height - field) / 2
    Seq(Trajectory.Q1, Trajectory.Q2, Trajectory.Q3, Trajectory.Q4).map: (x, y) =>
      (left + x * field, top + y * field)

  def draw(canvas: dom.HTMLCanvasElement, test: TestCase, phase: Double): Unit =
    val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val width = canvas.width.toDouble
    val height = canvas.height.toDouble
    context.fillStyle = Texture.paint(context, test.palette.ground)
    context.fillRect(0, 0, width, height)
    // Under the figure, so the bar and the square cover the marks they reach rather than being drawn over by them.
    crosses(context, width, height)
    // The figure is speckled too, from its own band. What moves across the frame is a textured object against a
    // textured ground, which is the thing a camera in a room is ever asked to see.
    val ink = Texture.paint(context, test.palette.ink)
    context.fillStyle = ink
    context.strokeStyle = ink

    // The trajectories live on a square field, centred in whatever the canvas turns out to be. Scaling x by the
    // width and y by the height independently would turn the circle into an ellipse the moment the canvas was not
    // exactly square -- which is a layout accident away, and was one.
    val field = math.min(width, height)
    val left = (width - field) / 2
    val top = (height - field) / 2
    def px(x: Double): Double = left + x * field
    def py(y: Double): Double = top + y * field

    test.figure match
      case Figure.Disc =>
        val (x, y) = Trajectory.circular(phase)
        context.beginPath()
        context.arc(px(x), py(y), field * 0.08, 0, 2 * math.Pi)
        context.fill()
      case Figure.Bar =>
        val (x, y) = Trajectory.swingingEnd(phase)
        context.lineWidth = field * 0.06
        context.lineCap = "round"
        context.beginPath()
        context.moveTo(px(Trajectory.Q3._1), py(Trajectory.Q3._2))
        context.lineTo(px(x), py(y))
        context.stroke()
      case Figure.Square =>
        val (x, y) = Trajectory.shuttle(phase)
        val side = field * 0.14
        context.fillRect(px(x) - side / 2, py(y) - side / 2, side, side)

private[fe] object Bench:
  /** Milliseconds to settle after a reset before the next test's movement begins.
    *
    * The reset travels to the other device and takes effect there; starting to move before it has would credit the
    * first reps of a new test to the tail of the last one.
    */
  val SettleAfterResetMillis = 1500

  /** How long to let a capture reach the backend before the buffer that produced it is wiped.
    *
    * The reset now clears the signal, so a capture asked for afterwards would record nothing. The acquirer reads its
    * buffer and posts a couple of hundred kilobytes over whatever connection a phone has, so this waits rather than
    * assuming: a lost recording costs the whole test that produced it.
    */
  val CaptureBeforeResetMillis = 3000
