package sgrv.fe.acquire

import org.scalajs.dom
import scala.scalajs.js

/** One acquisition sample: the mean brightness of each quadrant of a frame, indexed by [[Quadrant]]. */
private[fe] final case class Sample(atMillis: Double, quadrants: Map[Quadrant, Double]):
  def brightness(quadrant: Quadrant): Double = quadrants.getOrElse(quadrant, 0.0)

/** Turns the camera preview into the ~10 Hz signal the rep detector will run on.
  *
  * Each tick draws the current video frame into a deliberately tiny offscreen canvas and averages that instead of the
  * full frame: the browser downscales in hardware, so a sample costs a few thousand pixel reads rather than the best
  * part of a million. Nothing here needs detail — only how the brightness of four regions moves over time.
  */
private[fe] final class FrameSampler(
    video: dom.HTMLVideoElement,
    onSample: Sample => Unit,
    /** Read per frame, so the preview and the sampled frame stay in agreement the instant the toggle changes. */
    mirrored: () => Boolean = () => false,
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
      // Sampling follows the preview: when the image is mirrored for the viewer, the frame is flipped here too, so
      // the quadrant they see in the top right is the one Q1 measures.
      context.save()
      if mirrored() then
        context.translate(width.toDouble, 0)
        context.scale(-1, 1)
      context.asInstanceOf[js.Dynamic].drawImage(video, 0, 0, width, height)
      context.restore()
      val pixels = context.getImageData(0, 0, width, height).data
      val rgba = Array.tabulate(pixels.length)(pixels(_))
      onSample(Sample(js.Date.now(), FrameSampler.brightnesses(rgba, width, height)))

private[fe] object FrameSampler:
  /** ~10 Hz, per the acquisition design: far below the frame rate, and far above the 0.5-2 Hz band of interest. */
  val DefaultIntervalMillis = 100

  /** Roughly how many pixels a sample reads. A budget rather than a fixed edge: the camera's shape now follows the
    * device, and fixing the width alone would let a portrait frame cost three times as much per sample.
    */
  private[acquire] val SamplePixels = 2500

  /** Preserves the frame's aspect ratio so a quadrant stays a geometric quarter of what the camera sees, while holding
    * the cost of a sample roughly constant whatever shape that frame is.
    */
  private[acquire] def sampleSize(videoWidth: Int, videoHeight: Int): (Int, Int) =
    require(videoWidth > 0 && videoHeight > 0, "a frame must have a positive size")
    val scale = math.sqrt(SamplePixels.toDouble / (videoWidth.toDouble * videoHeight))
    def even(value: Double): Int = math.max(2, (math.round(value / 2) * 2).toInt)
    (even(videoWidth * scale), even(videoHeight * scale))

  /** Every quadrant's brightness for one frame. Four passes over a buffer of a couple of thousand pixels, which at 10
    * Hz costs far less than the one pass over the full frame it replaces.
    */
  private[acquire] def brightnesses(rgba: Array[Int], width: Int, height: Int): Map[Quadrant, Double] =
    Quadrant.All.map(quadrant => quadrant -> Quadrant.brightness(rgba, width, height, quadrant)).toMap
