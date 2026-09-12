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
  /** How many pixels a frame must carry at least.
    *
    * A floor, not a budget. It was a budget, and a ceiling on pixels is a thing worth trading field of view for, which
    * is the wrong trade entirely: a detector cannot count a movement the camera was not pointed at, and a sharper
    * picture of half the exercise is worth less than a coarser picture of all of it. So the frame is the smallest one
    * at the camera's own shape that still carries this many pixels, and if seeing everything costs more than this, it
    * costs more.
    *
    * A million is enough detail by a wide margin: a sample reads about two and a half thousand pixels, so even a
    * quadrant of this frame is two hundred times more than the detector looks at.
    */
  val MinimumPixels = 1_000_000

  /** The size asked for when the point is to see everything, not to see it sharply.
    *
    * Larger than any phone sensor, and deliberately the same in both dimensions. A camera's modes are not one picture
    * at several sizes: the wide ones are usually the tall one with its top and bottom discarded, so the mode with the
    * most field of view is the largest one whose shape is nearest square. Asking for a big square is exactly the
    * request that selects it, because the fitness distance a browser minimises counts the shortfall in each dimension
    * separately -- against a 4096 ideal, a 16:9 crop of a 4:3 sensor is always further away than the 4:3 mode it was
    * cut from, while the two are equally far from any request that names only a width.
    *
    * Opening without a size at all is what this replaces, and it is why the view stayed cropped: a browser left to
    * choose picks a convenient default, and on Android that default is a widescreen mode -- a crop, chosen before the
    * app has any say.
    */
  val FullFieldProbe = 4096

  /** Opens the camera asking for its whole field of view, and nothing else about the shape.
    *
    * With no camera chosen, the rear one is preferred: the acquirer points away from the user, at the equipment. A
    * chosen camera is requested exactly, since the point of choosing is to override that preference.
    */
  private[acquire] def openingConstraints(deviceId: Option[String]): dom.MediaStreamConstraints =
    val video = deviceId match
      case Some(id) => js.Dynamic.literal(deviceId = js.Dynamic.literal(exact = id))
      case None     => js.Dynamic.literal(facingMode = "environment")
    video.updateDynamic("width")(js.Dynamic.literal(ideal = FullFieldProbe))
    video.updateDynamic("height")(js.Dynamic.literal(ideal = FullFieldProbe))
    js.Dynamic.literal(audio = false, video = video).asInstanceOf[dom.MediaStreamConstraints]

  /** The same request on its own, for going back to the widest mode after a narrower one turned out to crop.
    *
    * No aspect ratio named here, unlike the request that steps down from it: naming one would be naming a shape, and
    * the point of this request is to accept whichever shape carries the most picture.
    */
  private def widestRequest: dom.MediaTrackConstraints =
    js.Dynamic
      .literal(
        width = js.Dynamic.literal(ideal = FullFieldProbe),
        height = js.Dynamic.literal(ideal = FullFieldProbe)
      )
      .asInstanceOf[dom.MediaTrackConstraints]

  /** Whether the frame should stand up or lie down: the way the screen does.
    *
    * A phone held upright has its sensor's long axis vertical, so the whole of what that camera can see is a tall
    * picture. A camera handing back a wide one in that position has not turned the picture round -- it has kept the
    * middle band and dropped the rest, which is precisely the field of view the exercise happens in.
    */
  private[acquire] def wantsPortrait(viewportWidth: Double, viewportHeight: Double): Boolean =
    viewportHeight >= viewportWidth

  /** The frame to ask for: the camera's own proportions, stood the way the screen is, carrying at least the minimum.
    *
    * The shape is taken from the widest mode the camera offers and only ever turned, never altered, so nothing is
    * cropped: a 4032x3024 sensor asked for portrait is asked for 3024x4032, which is the same picture rotated. Size is
    * then the smallest that meets the floor, because pixels past the floor buy nothing the detector can use and cost
    * throughput on a phone -- and it is capped at what the camera actually has, since asking for more than a sensor can
    * produce invites it to answer with some other mode entirely.
    *
    * Dimensions are rounded up to even, so the quadrant split is exact and the floor is met rather than just missed.
    */
  private[acquire] def wantedSize(
      nativeWidth: Int,
      nativeHeight: Int,
      portrait: Boolean,
      minimumPixels: Int
  ): (Int, Int) =
    require(nativeWidth > 0 && nativeHeight > 0, "a camera must report a positive size")
    val longer = math.max(nativeWidth, nativeHeight).toDouble
    val shorter = math.min(nativeWidth, nativeHeight).toDouble
    val aspect = longer / shorter
    val wanted = math.min(minimumPixels.toDouble, nativeWidth.toDouble * nativeHeight)
    val shortSide = math.sqrt(wanted / aspect)
    def even(value: Double): Int = math.max(2, (math.ceil(value / 2) * 2).toInt)
    val across = even(shortSide)
    val along = even(shortSide * aspect)
    if portrait then (across, along) else (along, across)

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

  /** Brings the frame down to the pixel budget without narrowing what it shows.
    *
    * The camera opened on its widest mode, which is more pixels than the detector can use and more than a phone can
    * decode all session without heating; but reducing the size means asking again, and asking again lets the browser
    * pick a different mode -- which is how field of view gets lost without anything appearing to go wrong. Two guards
    * against that. The request carries an aspect ratio alongside the dimensions, so a widescreen candidate is penalised
    * rather than merely not preferred. And the result is read back: if what came back is not the widest mode's own
    * shape, one way up or the other, the request is withdrawn and the wide mode restored.
    *
    * Field of view wins over pixel count whenever the two conflict. That is the whole point of the exercise: a detector
    * cannot count what the camera was not pointed at, and a sharper picture of half the movement is worth less than a
    * coarser picture of all of it.
    *
    * Best effort throughout: a browser that reports no size, or refuses the constraint, keeps the mode it opened with.
    */
  private def fitToDevice(stream: dom.MediaStream): Future[Unit] =
    stream.getVideoTracks().headOption match
      case None        => Future.successful(())
      case Some(track) =>
        deliveredSize(track) match
          case None         => Future.successful(())
          case Some(widest) =>
            val portrait = wantsPortrait(dom.window.innerWidth.toDouble, dom.window.innerHeight.toDouble)
            val (width, height) = wantedSize(widest._1, widest._2, portrait, MinimumPixels)
            track
              .applyConstraints(sizeRequest(width, height))
              .toFuture
              .flatMap: _ =>
                deliveredSize(track) match
                  case Some(now) if !keptFieldOfView(widest, now) =>
                    dom.console.warn(
                      s"Asking for ${width}x$height moved the camera off ${widest._1}x${widest._2} to " +
                        s"${now._1}x${now._2}, which is a different picture rather than the same one resized or " +
                        "turned; going back to the widest mode"
                    )
                    track.applyConstraints(widestRequest).toFuture.map(_ => ())
                  case Some(now) if portrait != (now._2 > now._1) =>
                    // Nothing lost, so this stands: the whole picture is there, lying the wrong way. Worth saying,
                    // because a preview that does not match the screen's shape looks like a fault and is not one.
                    dom.console.warn(
                      s"This camera will not turn its frame: asked for ${width}x$height, given ${now._1}x${now._2}. " +
                        "The whole field of view is there, in the other orientation."
                    )
                    Future.successful(())
                  case _ => Future.successful(())
              .recover { case _ => () }

  /** A request for a size that also says what shape that size is meant to be.
    *
    * The aspect ratio is the load-bearing part. Width and height alone leave a browser free to answer with a mode of
    * quite different proportions -- 1280x720 sits closer to a request for 1152x864 than 1024x768 does, by the distance
    * a browser actually measures -- and answering that way costs a quarter of the picture.
    */
  private def sizeRequest(width: Int, height: Int): dom.MediaTrackConstraints =
    js.Dynamic
      .literal(
        width = js.Dynamic.literal(ideal = width),
        height = js.Dynamic.literal(ideal = height),
        aspectRatio = js.Dynamic.literal(ideal = width.toDouble / height)
      )
      .asInstanceOf[dom.MediaTrackConstraints]

  /** Whether two frames show the same picture at different sizes, rather than different pictures.
    *
    * Proportions are the only evidence available: a camera does not report what it cropped, so a mode that comes back a
    * different shape from the one asked for has thrown something away, and a mode of the same shape has not.
    */
  private[acquire] def sameShape(before: (Int, Int), after: (Int, Int)): Boolean =
    val first = before._1.toDouble / before._2
    val second = after._1.toDouble / after._2
    first > 0 && second > 0 && math.abs(first - second) / math.max(first, second) <= ShapeTolerance

  /** How far two aspect ratios may differ and still count as the same shape: a couple of percent, which covers rounding
    * to even dimensions and nothing else. The gap between 4:3 and 16:9 is a third.
    */
  private[acquire] val ShapeTolerance = 0.02

  /** Whether a frame still shows everything the widest mode showed -- the same picture, resized, turned, or both.
    *
    * Turning is not losing. A sensor whose widest mode is 4032x3024 shows exactly as much at 3024x4032; the picture has
    * been stood on end, not trimmed. Any other shape has been trimmed, whatever its pixel count, and that is the only
    * thing this has to catch.
    */
  private[acquire] def keptFieldOfView(widest: (Int, Int), now: (Int, Int)): Boolean =
    sameShape(widest, now) || sameShape((widest._2, widest._1), now)

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
