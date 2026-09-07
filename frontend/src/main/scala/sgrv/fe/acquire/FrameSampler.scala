package sgrv.fe.acquire

import org.scalajs.dom
import scala.scalajs.js

/** One acquisition sample: the mean brightness of each quadrant of a frame, in the order top-left, top-right,
  * bottom-left, bottom-right.
  */
private[fe] final case class Sample(atMillis: Double, quadrants: Seq[Double])

/** Turns the camera preview into the ~10 Hz signal the rep detector will run on.
  *
  * Each tick draws the current video frame into a deliberately tiny offscreen canvas and averages that instead of the
  * full frame: the browser downscales in hardware, so a sample costs a few thousand pixel reads rather than the best
  * part of a million. Nothing here needs detail — only how the brightness of four regions moves over time.
  */
private[fe] final class FrameSampler(
    video: dom.HTMLVideoElement,
    onSample: Sample => Unit,
    intervalMillis: Int = FrameSampler.DefaultIntervalMillis
):
  private val canvas = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
  private var timer: Option[Int] = None

  def isRunning: Boolean = timer.isDefined

  def start(): Unit =
    if timer.isEmpty then timer = Some(dom.window.setInterval(() => tick(), intervalMillis.toDouble))

  def stop(): Unit =
    timer.foreach(dom.window.clearInterval)
    timer = None

  private def tick(): Unit =
    // HAVE_CURRENT_DATA. Before that the frame is not decoded yet and drawing it yields a blank sample.
    if video.readyState.asInstanceOf[Int] >= 2 && video.videoWidth > 0 && video.videoHeight > 0 then
      val (width, height) = FrameSampler.sampleSize(video.videoWidth, video.videoHeight)
      if canvas.width != width || canvas.height != height then
        canvas.width = width
        canvas.height = height
      val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
      context.asInstanceOf[js.Dynamic].drawImage(video, 0, 0, width, height)
      val pixels = context.getImageData(0, 0, width, height).data
      val rgba = Array.tabulate(pixels.length)(pixels(_))
      onSample(Sample(js.Date.now(), FrameSampler.quadrantLuma(rgba, width, height)))

private[fe] object FrameSampler:
  /** ~10 Hz, per the acquisition design: far below the frame rate, and far above the 0.5-2 Hz band of interest. */
  val DefaultIntervalMillis = 100

  /** The long edge of the offscreen canvas. Small on purpose; kept even so the quadrant split is exact. */
  private[acquire] val SampleWidth = 64

  /** Preserves the frame's aspect ratio so a quadrant stays a geometric quarter of what the camera sees. */
  private[acquire] def sampleSize(videoWidth: Int, videoHeight: Int): (Int, Int) =
    val scaled = math.round(SampleWidth.toDouble * videoHeight / videoWidth).toInt
    val height = math.max(2, scaled + (scaled % 2))
    (SampleWidth, height)

  /** Mean Rec.709 luma of each quadrant of an RGBA buffer, as top-left, top-right, bottom-left, bottom-right.
    *
    * Kept free of the DOM so the arithmetic can be tested directly. Odd dimensions are handled by giving the extra
    * row or column to the lower/right quadrants, which matters only for tiny frames.
    */
  private[acquire] def quadrantLuma(rgba: Array[Int], width: Int, height: Int): Seq[Double] =
    require(width > 0 && height > 0, "a frame must have a positive size")
    val midX = width / 2
    val midY = height / 2
    val totals = Array.fill(4)(0.0)
    val counts = Array.fill(4)(0)
    var y = 0
    while y < height do
      var x = 0
      while x < width do
        val offset = (y * width + x) * 4
        if offset + 2 < rgba.length then
          val luma = 0.2126 * rgba(offset) + 0.7152 * rgba(offset + 1) + 0.0722 * rgba(offset + 2)
          val quadrant = (if y < midY then 0 else 2) + (if x < midX then 0 else 1)
          totals(quadrant) += luma
          counts(quadrant) += 1
        x += 1
      y += 1
    Seq.tabulate(4)(index => if counts(index) == 0 then 0.0 else totals(index) / counts(index))
