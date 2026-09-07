package sgrv.fe.acquire

import org.scalajs.dom
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*

/** What the acquirer's camera is currently doing. Deliberately not part of `FrontendState`: a live `MediaStream` is a
  * browser resource that cannot be serialised, and a permission grant must be re-established on every page load
  * rather than remembered.
  */
private[fe] enum CameraState:
  case Idle
  case Starting
  case Streaming(width: Int, height: Int)
  case Unavailable(message: String)

private[fe] object Camera:
  /** 1280x720 is 921,600 pixels — just under the one-megapixel ceiling, and a 16:9 shape that fits a phone screen.
    * These are `ideal`, not `exact`: a camera that cannot honour them still opens, at whatever it does support,
    * rather than failing outright with OverconstrainedError.
    */
  val PreferredWidth = 1280
  val PreferredHeight = 720

  /** Built as a JS literal rather than through the typed facade: the shape of `MediaTrackConstraints` differs across
    * scalajs-dom versions, while the underlying object the browser wants is stable.
    */
  private[acquire] def constraints: dom.MediaStreamConstraints =
    js.Dynamic
      .literal(
        audio = false,
        video = js.Dynamic.literal(
          // The acquirer points away from the user, at the equipment, so prefer the rear camera where there is one.
          facingMode = "environment",
          width = js.Dynamic.literal(ideal = PreferredWidth),
          height = js.Dynamic.literal(ideal = PreferredHeight)
        )
      )
      .asInstanceOf[dom.MediaStreamConstraints]

  /** False on an insecure origin, where the browser does not expose `mediaDevices` at all. */
  private[acquire] def supported: Boolean =
    !js.isUndefined(dom.window.navigator.asInstanceOf[js.Dynamic].mediaDevices)

  def start(): Future[dom.MediaStream] =
    if !supported then Future.failed(CameraUnsupported())
    else dom.window.navigator.mediaDevices.getUserMedia(constraints)

  /** Releases the camera. Without this the indicator light stays on and the device stays locked to this tab. */
  def stop(stream: dom.MediaStream): Unit =
    stream.getTracks().foreach(_.stop())

  /** The stream's actual size, which may differ from what was asked for. */
  def resolution(stream: dom.MediaStream): Option[(Int, Int)] =
    stream.getVideoTracks().headOption.flatMap: track =>
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
