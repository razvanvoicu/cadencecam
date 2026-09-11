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

/** A camera control that can be held still, paired with the setting that says where to hold it. */
private[acquire] final case class ManualControl(mode: String, setting: String)

/** What became of the attempt to hold a camera's controls still, and why. */
private[fe] final case class ControlOutcome(
    reason: String,
    held: Seq[String] = Seq.empty,
    skipped: Seq[String] = Seq.empty
)

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

  def start(
      deviceId: Option[String] = None,
      // Called once the controls have settled, or once it is known they will not be touched. The caller can then
      // note when that happened against its own sample count, which is what puts the answer and the signal that
      // provoked the question in the same recording.
      onControls: ControlOutcome => Unit = _ => ()
  ): Future[dom.MediaStream] =
    if !supported then Future.failed(CameraUnsupported())
    else
      dom.window.navigator.mediaDevices
        .getUserMedia(openingConstraints(deviceId))
        .toFuture
        .flatMap: stream =>
          fitToDevice(stream).map: _ =>
            // Deliberately not awaited: the preview should appear at once, and the controls settle behind it.
            holdControlsStill(stream).foreach(onControls)
            stream

  /** The camera controls that must not move while a session runs.
    *
    * A camera left on automatic re-meters continuously, and the movement being counted is what it meters on. Every gain
    * change it makes shifts the whole frame together, so a rep in one quadrant appears as a wave in all four — a copy
    * of the signal arriving where the movement is not. That defeats the very check meant to validate a cadence, since
    * two quadrants can then agree on a period without either having seen the movement.
    *
    * Fixing it at the sensor is what the removed common-mode subtraction was reaching for and could not reach: a gain
    * change that never happens needs no undoing, whereas subtracting it afterwards mixed every channel into every
    * other.
    */
  private[acquire] val manualControls = Seq(
    ManualControl("exposureMode", "exposureTime"),
    ManualControl("whiteBalanceMode", "colorTemperature"),
    ManualControl("focusMode", "focusDistance")
  )

  /** How long the camera stays automatic before being held still.
    *
    * Locking the instant the stream opens freezes whatever the sensor started with, before metering has converged —
    * often far too dark or too bright to see anything in. A moment of automatic first gives it something worth holding.
    */
  private[acquire] val settleBeforeLockMillis = 1500

  /** Whether to hold the controls still at all. Off, pending evidence that it helps.
    *
    * It was added to stop the camera re-metering on the movement being counted, and a phone then began darkening badly
    * a second or so after opening — which looked exactly like this feature misfiring. It was not: a captured trace
    * showed the browser exposing neither `getCapabilities` nor `getSettings` on that device, so none of this ran, and
    * the darkening happened anyway. Rather than reason further about a suspect that cannot act, it is switched off so
    * the same test says plainly whether it was ever involved.
    */
  private[acquire] val holdControls = false

  /** Which of the wanted controls this camera says it can hold manually.
    *
    * The shape is checked rather than assumed. Browsers differ over what they report here, and casting an unexpected
    * value to an array of modes fails in a way no `Try` catches — it raises an `Error`, not an exception — so a camera
    * reporting something odd would take the whole capture down instead of simply going unlocked.
    */
  private[acquire] def manualCapable(capabilities: js.Dynamic): Seq[ManualControl] =
    manualControls.filter: control =>
      val modes = capabilities.selectDynamic(control.mode)
      !js.isUndefined(modes) &&
      js.Dynamic.global.Array.isArray(modes).asInstanceOf[Boolean] &&
      modes.asInstanceOf[js.Array[Any]].exists(_ == "manual")

  /** What to send to hold one control where it currently sits, or nothing when it cannot be held there.
    *
    * The mode alone is worse than useless. "Manual" tells the camera to stop deciding, and a request carrying no value
    * does not say what to do instead, so the camera chooses — and what it chooses is nothing in particular. That is how
    * a correctly exposed picture turns dark a second after opening.
    *
    * Cameras commonly advertise a manual mode in their capabilities while reporting no current value for it in their
    * settings, which is exactly the case that goes wrong. So a control whose value cannot be read is left alone: an
    * automatic exposure that drifts is a nuisance, an exposure pinned to a number nobody chose is unusable.
    */
  private[acquire] def pinning(control: ManualControl, settings: js.Dynamic): Option[js.Dynamic] =
    val current = settings.selectDynamic(control.setting)
    Option.when(!js.isUndefined(current) && current != null):
      val wanted = js.Dynamic.literal()
      wanted.updateDynamic(control.mode)("manual")
      wanted.updateDynamic(control.setting)(current)
      wanted

  /** What this camera says it can do and where it currently sits, as JSON.
    *
    * Carried on a captured trace, because these differ by device in ways that decide whether holding the controls still
    * works at all, and reading them off a phone any other way is guesswork.
    */
  private[fe] def report(stream: dom.MediaStream): Option[String] =
    stream
      .getVideoTracks()
      .headOption
      .map: track =>
        val dynamic = track.asInstanceOf[js.Dynamic]
        val hasCapabilities = !js.isUndefined(dynamic.getCapabilities)
        val hasSettings = !js.isUndefined(dynamic.getSettings)
        val described = js.Dynamic.literal()
        // Recorded whether or not they exist. An absent field used to mean either "old build" or "browser does not
        // offer this", and being unable to tell those apart is what sent two diagnoses in the wrong direction.
        described.updateDynamic("hasGetCapabilities")(hasCapabilities)
        described.updateDynamic("hasGetSettings")(hasSettings)
        if hasCapabilities then described.updateDynamic("capabilities")(dynamic.getCapabilities())
        if hasSettings then described.updateDynamic("settings")(dynamic.getSettings())
        try js.JSON.stringify(described)
        catch case _: Throwable => s"""{"hasGetCapabilities":$hasCapabilities,"hasGetSettings":$hasSettings}"""

  /** Holds each supported control still, once the camera has had a moment to meter.
    *
    * Best effort throughout, and one control at a time: a camera willing to hold exposure but not focus should still
    * hold exposure, and a browser supporting none of this keeps working exactly as it did.
    */
  private def holdControlsStill(stream: dom.MediaStream): Future[ControlOutcome] =
    stream.getVideoTracks().headOption match
      case None               => Future.successful(ControlOutcome("no video track"))
      case _ if !holdControls => Future.successful(ControlOutcome("switched off"))
      case Some(track)        =>
        val dynamic = track.asInstanceOf[js.Dynamic]
        if js.isUndefined(dynamic.getCapabilities) then
          Future.successful(ControlOutcome("this browser does not report camera capabilities"))
        else if js.isUndefined(dynamic.getSettings) then
          Future.successful(ControlOutcome("this browser does not report camera settings"))
        else
          after(settleBeforeLockMillis).flatMap: _ =>
            // Read after the settle, not before: what gets pinned should be what automatic metering arrived at.
            val settled = dynamic.getSettings()
            val capable = manualCapable(dynamic.getCapabilities())
            val skipped = capable.filter(control => pinning(control, settled).isEmpty).map(_.mode)
            if skipped.nonEmpty then
              dom.console.info(s"Left automatic, having no value to hold them at: ${skipped.mkString(", ")}")
            capable
              .flatMap(control => pinning(control, settled).map(control.mode -> _))
              .foldLeft(Future.successful(Seq.empty[String])): (earlier, pinned) =>
                val (mode, wanted) = pinned
                earlier.flatMap: locked =>
                  track
                    .applyConstraints(
                      js.Dynamic.literal(advanced = js.Array(wanted)).asInstanceOf[dom.MediaTrackConstraints]
                    )
                    .toFuture
                    .map(_ => locked :+ mode)
                    .recover { case _ => locked }
              .map: locked =>
                if locked.isEmpty then dom.console.info("The camera holds none of its controls still")
                else dom.console.info(s"Camera controls held still: ${locked.mkString(", ")}")
                ControlOutcome("attempted", locked, skipped)

  private def after(millis: Int): Future[Unit] =
    val settled = scala.concurrent.Promise[Unit]()
    dom.window.setTimeout(() => settled.success(()), millis.toDouble)
    settled.future

  /** Narrows an open camera to the largest mode within the budget at the device's own aspect ratio.
    *
    * Best effort by design: a browser that does not report its capabilities, or refuses the constraint, simply keeps
    * the mode it opened with. A working camera at the wrong size beats no camera at all.
    */
  private def fitToDevice(stream: dom.MediaStream): Future[Unit] =
    stream.getVideoTracks().headOption match
      case None        => Future.successful(())
      case Some(track) =>
        deliveredSize(track) match
          case None                                    => Future.successful(())
          case Some((deliveredWidth, deliveredHeight)) =>
            val (maxWidth, maxHeight) = deviceMaximum(track).getOrElse((Int.MaxValue, Int.MaxValue))
            val (width, height) = bestSize(deliveredWidth, deliveredHeight, maxWidth, maxHeight, PixelBudget)
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

  /** The shape the camera is actually delivering, which is the one to keep.
    *
    * Taken from the track's own settings rather than from its capabilities. A capability reports the largest width and
    * the largest height the camera can manage, and those are two separate numbers: on a phone they describe the sensor
    * laid out landscape, and they need not even belong to the same supported mode. Building a request out of them asked
    * a portrait camera for a landscape frame, which it can only satisfy by throwing away field of view. What it is
    * already sending has the proportions the device actually wants.
    */
  private def deliveredSize(track: dom.MediaStreamTrack): Option[(Int, Int)] =
    val dynamic = track.asInstanceOf[js.Dynamic]
    if js.isUndefined(dynamic.getSettings) then None
    else
      val settings = dynamic.getSettings()
      for
        width <- Option(settings.width).filterNot(js.isUndefined).map(_.asInstanceOf[Int]).filter(_ > 0)
        height <- Option(settings.height).filterNot(js.isUndefined).map(_.asInstanceOf[Int]).filter(_ > 0)
      yield (width, height)

  /** The largest frame that keeps the camera's own proportions, within the pixel budget and the camera's own limits.
    *
    * The proportions come from what is being delivered and are never altered, so the whole field of view survives: only
    * the number of pixels it is described with changes. The camera's maxima are used as bounds rather than as a shape,
    * which is all they can honestly be.
    */
  private[acquire] def bestSize(
      deliveredWidth: Int,
      deliveredHeight: Int,
      maxWidth: Int,
      maxHeight: Int,
      budget: Int
  ): (Int, Int) =
    require(deliveredWidth > 0 && deliveredHeight > 0, "a camera must report a positive size")
    val aspect = deliveredWidth.toDouble / deliveredHeight
    val fromBudget = math.sqrt(budget * aspect)
    val width = math.min(fromBudget, math.min(maxWidth.toDouble, maxHeight.toDouble * aspect))
    // Rounded down and kept even, so the quadrant split is exact and the frame cannot creep back over the budget.
    def even(value: Double): Int = math.max(2, (math.floor(value / 2) * 2).toInt)
    (even(width), even(width / aspect))

  /** The largest frame the device says it can produce. Two independent maxima, so useful only as bounds. */
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

  /** Lets a video element go of whatever stream it was showing.
    *
    * Stopping a track ends the capture, but an element still holding the stream keeps a reference the browser is
    * entitled to honour, and on a phone that shows up as a camera that stays on after leaving the screen. Pausing
    * first, then clearing the source, is the order that leaves nothing behind.
    */
  def detach(element: dom.HTMLVideoElement): Unit =
    try
      element.pause()
      element.asInstanceOf[js.Dynamic].srcObject = null
      element.removeAttribute("src")
      element.load()
    catch case _: Throwable => ()

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
