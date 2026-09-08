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

  /** Opens the camera without dictating a size, so the device offers its own preferred mode and its own aspect.
    *
    * With no camera chosen, the rear one is preferred: the acquirer points away from the user, at the equipment. A
    * chosen camera is requested exactly, since the point of choosing is to override that preference.
    */
  private[acquire] def openingConstraints(deviceId: Option[String]): dom.MediaStreamConstraints =
    val video = deviceId match
      case Some(id) => js.Dynamic.literal(deviceId = js.Dynamic.literal(exact = id))
      case None     => js.Dynamic.literal(facingMode = "environment")
    js.Dynamic.literal(audio = false, video = video).asInstanceOf[dom.MediaStreamConstraints]

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

  def start(deviceId: Option[String] = None): Future[dom.MediaStream] =
    if !supported then Future.failed(CameraUnsupported())
    else
      dom.window.navigator.mediaDevices
        .getUserMedia(openingConstraints(deviceId))
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

  /** Whether the picture should be flipped for the viewer.
    *
    * A camera on the same side as the screen shows the viewer to themselves, and people expect that reversed, the way a
    * mirror is. A camera facing away shows the world, which must not be reversed.
    *
    * An unknown facing is treated as user-facing: cameras that decline to say are overwhelmingly the built-in one on a
    * laptop, which points at the person using it. A rear phone camera always identifies itself.
    */
  private[fe] def mirrors(facing: Option[String]): Boolean = !facing.contains("environment")

  /** Which way the open camera points, as the track itself reports it. */
  def facing(stream: dom.MediaStream): Option[String] =
    setting(stream, "facingMode").map(_.toString)

  def deviceIdOf(stream: dom.MediaStream): Option[String] =
    setting(stream, "deviceId").map(_.toString).filter(_.nonEmpty)

  private def setting(stream: dom.MediaStream, name: String): Option[js.Any] =
    stream
      .getVideoTracks()
      .headOption
      .flatMap: track =>
        val settings = track.asInstanceOf[js.Dynamic].getSettings()
        Option(settings.selectDynamic(name)).filterNot(js.isUndefined).map(_.asInstanceOf[js.Any])

  /** Every camera this device exposes. Empty when the browser offers no enumeration at all. */
  def videoInputs(): Future[Seq[CameraDevice]] =
    val devices = dom.window.navigator.asInstanceOf[js.Dynamic].mediaDevices
    if js.isUndefined(devices) || js.isUndefined(devices.enumerateDevices) then Future.successful(Seq.empty)
    else
      devices
        .enumerateDevices()
        .asInstanceOf[js.Promise[js.Array[js.Dynamic]]]
        .toFuture
        .map: found =>
          found.toSeq
            .filter(device => device.kind.asInstanceOf[String] == "videoinput")
            .map(device => CameraDevice(device.deviceId.asInstanceOf[String], device.label.asInstanceOf[String]))
            .filter(_.deviceId.nonEmpty)
        .recover { case _ => Seq.empty }

  /** The camera after this one, wrapping around; `None` when there is nothing to switch to. */
  def nextDevice(devices: Seq[CameraDevice], current: Option[String]): Option[CameraDevice] =
    if devices.sizeIs < 2 then None
    else
      val index = current.flatMap(id => Option(devices.indexWhere(_.deviceId == id)).filter(_ >= 0)).getOrElse(-1)
      Some(devices((index + 1) % devices.size))

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

/** A camera this device offers. The label is only populated once permission has been granted, which is why the list is
  * read after the first stream opens rather than before.
  */
private[fe] final case class CameraDevice(deviceId: String, label: String)

private[fe] object CameraDevice:
  /** A readable name for a camera, falling back to its position in the list when the browser gives none. */
  def nameOf(device: CameraDevice, index: Int): String =
    Option(device.label).map(_.trim).filter(_.nonEmpty).getOrElse(s"Camera ${index + 1}")
