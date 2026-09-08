package sgrv.fe.acquire

import org.scalajs.dom
import scala.concurrent.Future
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global

/** What the acquirer's camera is currently doing. Deliberately not part of `FrontendState`: a live `MediaStream` is a
  * browser resource that cannot be serialised, and a permission grant must be re-established on every page load rather
  * than remembered.
  */
private[fe] enum CameraState:
  case Idle
  case Starting
  case Streaming(width: Int, height: Int)
  case Unavailable(message: String)

private[fe] object Camera:
  /** How many pixels a frame may carry. A budget rather than a fixed size: the shape is left to the device.
    *
    * Asking for a particular width and height pins an aspect ratio, and a phone whose sensor is 4:3 satisfies a 16:9
    * request by cropping — quietly discarding field of view, which is the one thing this app cannot spare. It needs to
    * see the whole movement, not a sharper picture of part of it.
    */
  val PixelBudget = 1_000_000

  /** Opens the camera without dictating a size, so the device offers its own preferred mode and its own aspect. */
  private[acquire] def openingConstraints: dom.MediaStreamConstraints =
    js.Dynamic
      .literal(
        audio = false,
        // The acquirer points away from the user, at the equipment, so prefer the rear camera where there is one.
        video = js.Dynamic.literal(facingMode = "environment")
      )
      .asInstanceOf[dom.MediaStreamConstraints]

  /** The largest frame within the budget that keeps the device's own proportions.
    *
    * Scaling both dimensions by the same factor is what preserves the field of view: the frame carries fewer pixels but
    * still shows everything the sensor can see. Dimensions are kept even so the quadrant split is exact.
    */
  private[acquire] def budgetedSize(deviceWidth: Int, deviceHeight: Int, budget: Int): (Int, Int) =
    require(deviceWidth > 0 && deviceHeight > 0, "a camera must report a positive size")
    val pixels = deviceWidth.toDouble * deviceHeight
    val scale = if pixels <= budget then 1.0 else math.sqrt(budget / pixels)
    // Rounded down, not to nearest: rounding up can carry the frame back over the budget it was scaled to meet.
    def even(value: Double): Int = math.max(2, (math.floor(value / 2) * 2).toInt)
    (even(deviceWidth * scale), even(deviceHeight * scale))

  /** False on an insecure origin, where the browser does not expose `mediaDevices` at all. */
  private[acquire] def supported: Boolean =
    !js.isUndefined(dom.window.navigator.asInstanceOf[js.Dynamic].mediaDevices)

  def start(): Future[dom.MediaStream] =
    if !supported then Future.failed(CameraUnsupported())
    else
      dom.window.navigator.mediaDevices
        .getUserMedia(openingConstraints)
        .toFuture
        .flatMap(stream => fitToDevice(stream).map(_ => stream))

  /** Narrows an open camera to the largest mode within the budget at the device's own aspect ratio.
    *
    * Best effort by design: a browser that does not report its capabilities, or refuses the constraint, simply keeps
    * the mode it opened with. A working camera at the wrong size beats no camera at all.
    */
  private def fitToDevice(stream: dom.MediaStream): Future[Unit] =
    stream.getVideoTracks().headOption match
      case None        => Future.successful(())
      case Some(track) =>
        deviceMaximum(track) match
          case None                              => Future.successful(())
          case Some((deviceWidth, deviceHeight)) =>
            val (width, height) = budgetedSize(deviceWidth, deviceHeight, PixelBudget)
            val wanted = js.Dynamic
              .literal(
                width = js.Dynamic.literal(ideal = width),
                height = js.Dynamic.literal(ideal = height)
              )
              .asInstanceOf[dom.MediaTrackConstraints]
            track
              .applyConstraints(wanted)
              .toFuture
              .map(_ => ())
              .recover { case _ => () }

  /** The largest frame the device says it can produce, which carries its native proportions. */
  private def deviceMaximum(track: dom.MediaStreamTrack): Option[(Int, Int)] =
    val dynamic = track.asInstanceOf[js.Dynamic]
    if js.isUndefined(dynamic.getCapabilities) then None
    else
      val capabilities = dynamic.getCapabilities()
      for
        width <- Option(capabilities.width).filterNot(js.isUndefined).flatMap(value => maximumOf(value))
        height <- Option(capabilities.height).filterNot(js.isUndefined).flatMap(value => maximumOf(value))
      yield (width, height)

  private def maximumOf(range: js.Dynamic): Option[Int] =
    Option(range.max).filterNot(js.isUndefined).map(_.asInstanceOf[Int]).filter(_ > 0)

  /** Releases the camera. Without this the indicator light stays on and the device stays locked to this tab. */
  def stop(stream: dom.MediaStream): Unit =
    stream.getTracks().foreach(_.stop())

  /** The stream's actual size, which may differ from what was asked for. */
  def resolution(stream: dom.MediaStream): Option[(Int, Int)] =
    stream
      .getVideoTracks()
      .headOption
      .flatMap: track =>
        val settings = track.asInstanceOf[js.Dynamic].getSettings()
        for
          width <- Option(settings.width.asInstanceOf[js.UndefOr[Int]]).flatMap(_.toOption)
          height <- Option(settings.height.asInstanceOf[js.UndefOr[Int]]).flatMap(_.toOption)
        yield (width, height)

  private[acquire] final case class CameraUnsupported()
      extends RuntimeException("This browser exposes no camera on an insecure connection")

  /** Turns a getUserMedia rejection into something a person can act on. The browser's own messages are terse and
    * inconsistent between engines, while the `name` is well defined.
    */
  private[fe] def failureMessage(error: Throwable): String =
    error match
      case _: CameraUnsupported =>
        "This page needs a secure connection to use the camera. Open it over HTTPS, or on localhost — a plain " +
          "http:// address on the local network will not work."
      case other =>
        errorName(other) match
          case "NotAllowedError" | "PermissionDeniedError" =>
            "Camera access was refused. Allow the camera for this site in your browser settings, then try again."
          case "NotFoundError" | "DevicesNotFoundError" => "No camera was found on this device."
          case "NotReadableError" | "TrackStartError"   =>
            "The camera is already in use by another app. Close it and try again."
          case "OverconstrainedError" | "ConstraintNotSatisfiedError" =>
            "No camera on this device can meet the requested video format."
          case "SecurityError" => "Camera access is blocked by this browser's security settings."
          case _               =>
            Option(other.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse("The camera could not be started.")

  private def errorName(error: Throwable): String =
    error match
      case js.JavaScriptException(value) =>
        Option(value.asInstanceOf[js.Dynamic].name).map(_.toString).getOrElse("")
      case _ => ""
