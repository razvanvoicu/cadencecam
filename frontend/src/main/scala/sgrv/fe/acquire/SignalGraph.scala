package sgrv.fe.acquire

import org.scalajs.dom

/** Draws one quadrant's recent samples as a scrolling trace: oldest at the left edge, newest at the right.
  *
  * A debugging aid for validating the acquisition end to end — that the camera is sampled steadily, that movement in
  * front of a quadrant shows up in that quadrant's series, and that the timing looks like 10 Hz. Not part of the
  * detector, and expected to be switched off once it has done its job.
  */
private[fe] object SignalGraph:
  /** Five seconds of a 10 Hz signal. Wide enough to show several cycles of a 0.5-2 Hz movement. */
  val WindowSamples = 50

  /** The narrowest range a pane will draw against, out of the 0-255 brightness scale.
    *
    * Without a floor, a window scaled to its own extent turns a still scene's sensor noise into a full-height scribble:
    * the smaller the real variation, the more it is magnified. Holding the range open to 16 units means a camera
    * looking at nothing draws very nearly a flat line, and only a movement worth several units of brightness begins to
    * fill the pane.
    */
  private[acquire] val MinimumRange = 16.0

  /** The value range a pane draws against: the window's own extent, so a trace fills the height available to it, but
    * never narrower than [[MinimumRange]].
    *
    * Above that floor, heights are not comparable between panes or over time — a modest movement in a quiet window
    * draws as tall as a large one in a busy window. That is the trade accepted for legibility: these traces show that a
    * movement is being seen and roughly how it is shaped, not how big it is.
    */
  private[acquire] def range(samples: Seq[Double]): (Double, Double) =
    if samples.isEmpty then (0.0, MinimumRange)
    else
      val low = samples.min
      val high = samples.max
      if high - low >= MinimumRange then (low, high)
      else
        val middle = (low + high) / 2
        (middle - MinimumRange / 2, middle + MinimumRange / 2)

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

  def draw(canvas: dom.HTMLCanvasElement, traces: Seq[Trace], grid: String): Unit =
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
        val (low, high) = range(trace.samples)
        val span = math.max(high - low, 1e-9)
        context.strokeStyle = trace.stroke
        context.lineWidth = trace.width
        context.beginPath()
        trace.samples.zipWithIndex.foreach: (value, index) =>
          val x = positionOf(index) * width
          val y = padding + (1.0 - (value - low) / span) * usable
          if index == 0 then context.moveTo(x, y) else context.lineTo(x, y)
        context.stroke()
