package sgrv.fe.acquire

import org.scalajs.dom
import zio.json.{DeriveJsonCodec, JsonCodec}

/** Draws one quadrant's recent samples as a scrolling trace: oldest at the left edge, newest at the right.
  *
  * A debugging aid for validating the acquisition end to end — that the camera is sampled steadily, that movement in
  * front of a quadrant shows up in that quadrant's series, and that the timing looks like 10 Hz. Not part of the
  * detector, and expected to be switched off once it has done its job.
  */
private[fe] object SignalGraph:
  /** Five seconds of a 10 Hz signal. Wide enough to show several cycles of a 0.5-2 Hz movement. */
  val WindowSamples = 50

  /** Full brightness scale, the absolute reference. */
  private[acquire] val ScaleLow = 0.0
  private[acquire] val ScaleHigh = 255.0

  /** The value range a pane draws against, for the chosen zoom.
    *
    * Every step is a fixed, stated span, so a given change in brightness always draws the same height at a given zoom
    * and the panes stay comparable with each other. What changes with the data is only where that span sits: it is
    * centred on the window's own mean, so slow drift does not push the trace off the pane. The span never widens to fit
    * the data, which is what separates this from scaling each window to its own extent.
    *
    * Every step is centred, including the widest. An absolute 0-255 window would suit the raw channel but not a
    * common-mode-corrected one, whose values are deviations about zero and would sit off the bottom of the pane.
    */
  private[acquire] def range(samples: Seq[Double], zoom: SignalZoom): (Double, Double) =
    val span = SignalZoom.span(zoom)
    val centre = if samples.isEmpty then (ScaleLow + ScaleHigh) / 2 else samples.sum / samples.size
    (centre - span, centre + span)

  /** Horizontal position of a sample, in the range 0 to 1.
    *
    * The window is always [[WindowSamples]] wide, so while the buffer is still filling the trace grows rightwards from
    * the left edge, and only once full does it start sliding — which is what makes the motion readable rather than a
    * line that rescales under you.
    */
  private[acquire] def positionOf(index: Int, windowSamples: Int = WindowSamples): Double =
    if windowSamples <= 1 then 0.0 else index.toDouble / (windowSamples - 1)

  /** One line to draw. Each trace is centred on its own mean, so series with different resting levels can share a pane
    * and be compared by shape.
    */
  final case class Trace(samples: Seq[Double], stroke: String, width: Double)

  def draw(canvas: dom.HTMLCanvasElement, traces: Seq[Trace], zoom: SignalZoom, grid: String): Unit =
    val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val width = canvas.width.toDouble
    val height = canvas.height.toDouble
    context.clearRect(0, 0, width, height)

    // A midline gives the eye something to judge the wave against without needing axes.
    context.strokeStyle = grid
    context.lineWidth = 1
    context.beginPath()
    context.moveTo(0, height / 2)
    context.lineTo(width, height / 2)
    context.stroke()

    val padding = 2.0
    val usable = math.max(height - 2 * padding, 1.0)
    traces
      .filter(_.samples.nonEmpty)
      .foreach: trace =>
        val (low, high) = range(trace.samples, zoom)
        val span = math.max(high - low, 1e-9)
        context.strokeStyle = trace.stroke
        context.lineWidth = trace.width
        context.beginPath()
        trace.samples.zipWithIndex.foreach: (value, index) =>
          val x = positionOf(index) * width
          val y = padding + (1.0 - (value - low) / span) * usable
          if index == 0 then context.moveTo(x, y) else context.lineTo(x, y)
        context.stroke()

/** How much of the brightness scale a trace shows, as a half-width about the series' own mean.
  *
  * Fixed, stated spans rather than a fit to the data: the point of the graphs is to judge how large a movement is,
  * which needs the height of a step to mean the same thing from one moment to the next. ±128 spans the whole 0-255
  * range, so it is the widest view worth having.
  */
private[fe] enum SignalZoom:
  case Span128, Span32, Span8, Span2

private[fe] object SignalZoom:
  given JsonCodec[SignalZoom] = DeriveJsonCodec.gen[SignalZoom]

  /** Half-width of the visible band about the series' mean. */
  def span(zoom: SignalZoom): Double =
    zoom match
      case Span128 => 128.0
      case Span32  => 32.0
      case Span8   => 8.0
      case Span2   => 2.0

  def label(zoom: SignalZoom): String = s"±${span(zoom).toInt}"

  /** Cycles widest to narrowest and back, so one control covers every step. */
  def next(zoom: SignalZoom): SignalZoom =
    zoom match
      case Span128 => Span32
      case Span32  => Span8
      case Span8   => Span2
      case Span2   => Span128
