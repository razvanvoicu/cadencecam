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

private[fe] final case class Outcome(test: String, reference: Int, acquired: Int, passed: Boolean)

/** Draws the moving figure of a test onto a canvas.
  *
  * Everything is in fractions of the canvas, so what the camera sees depends on how the screen is framed rather than on
  * the pixel size of any element -- and the quadrants the trajectories are described in are the quadrants the detector
  * will divide its own view into, provided the camera frames this canvas and not the whole page.
  */
private[fe] object Painter:
  def draw(canvas: dom.HTMLCanvasElement, test: TestCase, phase: Double): Unit =
    val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val width = canvas.width.toDouble
    val height = canvas.height.toDouble
    context.fillStyle = test.palette.ground
    context.fillRect(0, 0, width, height)
    context.fillStyle = test.palette.ink
    context.strokeStyle = test.palette.ink

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
