package sgrv.fe.browser

import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js

/** The browser shapes that scalajs-dom either does not describe, or describes as `js.Any`.
  *
  * Camera capabilities are deliberately open dictionaries: vendors add controls before the browser typings know about
  * them. The rest of the frontend should not have to become dynamic as a result, so all inspection and request
  * construction lives here and is exposed as the small typed vocabulary below.
  */
private[fe] object CameraInterop:

  enum Property:
    case Number(value: Double)
    case Text(value: String)
    case Modes(values: Seq[String])
    case Range(min: Double, max: Double, step: Option[Double] = None)

  enum Setting:
    case Number(value: Double)
    case Text(value: String)

  final case class Constraint private (values: Map[String, Setting]):
    def keys: Seq[String] = values.keys.toSeq
    def number(name: String): Option[Double] = values.get(name).collect { case Setting.Number(value) => value }
    def contains(name: String): Boolean = values.contains(name)
    def text(name: String): Option[String] = values.get(name).collect { case Setting.Text(value) => value }

  object Constraint:
    val empty: Constraint = Constraint(Map.empty)
    def number(name: String, value: Double): Constraint = Constraint(Map(name -> Setting.Number(value)))
    def text(name: String, value: String): Constraint = Constraint(Map(name -> Setting.Text(value)))
    def apply(values: (String, Setting)*): Constraint = Constraint(values.toMap)

  final case class NumericRange(min: Option[Double], max: Option[Double], step: Option[Double])

  final class Properties private (private[CameraInterop] val raw: js.Dictionary[js.Any]):
    private def value(name: String): Option[js.Any] =
      raw.get(name).filter(value => value != null && !js.isUndefined(value))

    def number(name: String): Option[Double] =
      value(name)
        .filter(js.typeOf(_) == "number")
        .map(_.asInstanceOf[Double])
        .filterNot(_.isNaN)

    def text(name: String): Option[String] = value(name).filter(js.typeOf(_) == "string").map(_.toString)

    def setting(name: String): Option[Setting] =
      number(name).map(Setting.Number.apply).orElse(text(name).map(Setting.Text.apply))

    def modes(name: String): Seq[String] =
      value(name)
        .filter(value => js.Array.isArray(value))
        .map(_.asInstanceOf[js.Array[js.Any]].toSeq.collect {
          case value if js.typeOf(value) == "string" => value.toString
        })
        .getOrElse(Seq.empty)

    def range(name: String): Option[NumericRange] =
      value(name)
        .filter(js.typeOf(_) == "object")
        .filterNot(js.Array.isArray)
        .map(_.asInstanceOf[js.Dictionary[js.Any]])
        .map: span =>
          val values = new Properties(span)
          NumericRange(values.number("min"), values.number("max"), values.number("step"))

    def same(names: Seq[String], other: Properties): Boolean =
      names.forall: name =>
        (value(name), other.value(name)) match
          case (None, None)       => true
          case (Some(a), Some(b)) => js.JSON.stringify(a) == js.JSON.stringify(b)
          case _                  => false

    def json: String = js.JSON.stringify(raw)

  object Properties:
    val empty: Properties = new Properties(js.Dictionary.empty[js.Any])

    /** A typed constructor used by the camera policy tests and by any synthetic browser adapter. */
    def apply(entries: (String, Property)*): Properties =
      val raw = js.Dictionary.empty[js.Any]
      entries.foreach: (name, property) =>
        raw(name) = property match
          case Property.Number(value)         => value
          case Property.Text(value)           => value
          case Property.Modes(values)         => js.Array(values*)
          case Property.Range(min, max, step) =>
            val range = js.Dictionary[js.Any]("min" -> min, "max" -> max)
            step.foreach(value => range("step") = value)
            range
      new Properties(raw)

    private[CameraInterop] def from(raw: js.Any): Properties =
      new Properties(raw.asInstanceOf[js.Dictionary[js.Any]])

  final class Track private[CameraInterop] (private val track: dom.MediaStreamTrack):
    private val dynamic = track.asInstanceOf[js.Dynamic]

    def hasCapabilities: Boolean = !js.isUndefined(dynamic.getCapabilities)
    def hasSettings: Boolean = !js.isUndefined(dynamic.getSettings)

    def capabilities: Option[Properties] =
      if !hasCapabilities then None else properties(dynamic.getCapabilities())

    def settings: Option[Properties] =
      if !hasSettings then None else properties(dynamic.getSettings())

    private def properties(value: js.Any): Option[Properties] =
      Option(value).filterNot(js.isUndefined).map(Properties.from)

    def apply(wanted: Constraint): Future[Unit] =
      val values = js.Dynamic.literal()
      wanted.values.foreach:
        case (name, Setting.Number(value)) => values.updateDynamic(name)(value)
        case (name, Setting.Text(value))   => values.updateDynamic(name)(value)
      track
        .applyConstraints(js.Dynamic.literal(advanced = js.Array(values)).asInstanceOf[dom.MediaTrackConstraints])
        .toFuture
        .map(_ => ())

    def report: String =
      val described = js.Dynamic.literal(
        hasGetCapabilities = hasCapabilities,
        hasGetSettings = hasSettings
      )
      capabilities.foreach(value => described.updateDynamic("capabilities")(value.raw))
      settings.foreach(value => described.updateDynamic("settings")(value.raw))
      try js.JSON.stringify(described)
      catch case _: Throwable => s"""{"hasGetCapabilities":$hasCapabilities,"hasGetSettings":$hasSettings}"""

  def track(track: dom.MediaStreamTrack): Track = new Track(track)

  def firstVideoTrack(stream: dom.MediaStream): Option[Track] = stream.getVideoTracks().headOption.map(track)

  def openingConstraints(deviceId: Option[String], fullFieldProbe: Int): dom.MediaStreamConstraints =
    val video = deviceId match
      case Some(id) => js.Dynamic.literal(deviceId = js.Dynamic.literal(exact = id))
      case None     => js.Dynamic.literal(facingMode = "environment")
    video.updateDynamic("width")(js.Dynamic.literal(ideal = fullFieldProbe))
    video.updateDynamic("height")(js.Dynamic.literal(ideal = fullFieldProbe))
    // `none` restricts selection to a mode the camera, driver or operating system actually offers. Without it the
    // browser is explicitly allowed to manufacture the requested shape by cropping a larger frame, which defeats a
    // request whose purpose is to retain the field of view. An unknown constraint is ignored by older browsers; a
    // browser that implements resizeMode must offer `none`.
    video.updateDynamic("resizeMode")(js.Dynamic.literal(exact = "none"))
    js.Dynamic.literal(audio = false, video = video).asInstanceOf[dom.MediaStreamConstraints]

  def widestConstraints(fullFieldProbe: Int): dom.MediaTrackConstraints =
    js.Dynamic
      .literal(
        width = js.Dynamic.literal(ideal = fullFieldProbe),
        height = js.Dynamic.literal(ideal = fullFieldProbe),
        resizeMode = js.Dynamic.literal(exact = "none")
      )
      .asInstanceOf[dom.MediaTrackConstraints]

  def sizeConstraints(width: Int, height: Int): dom.MediaTrackConstraints =
    js.Dynamic
      .literal(
        width = js.Dynamic.literal(ideal = width),
        height = js.Dynamic.literal(ideal = height),
        aspectRatio = js.Dynamic.literal(ideal = width.toDouble / height),
        resizeMode = js.Dynamic.literal(exact = "none")
      )
      .asInstanceOf[dom.MediaTrackConstraints]

  /** False on an insecure origin, where the property itself is absent. */
  def mediaDevicesAvailable: Boolean =
    val devices = dom.window.navigator.asInstanceOf[js.Dynamic].mediaDevices
    !js.isUndefined(devices) && devices != null

  final case class VideoInput(deviceId: String, label: String)

  def videoInputs(): Future[Seq[VideoInput]] =
    val devices = dom.window.navigator.asInstanceOf[js.Dynamic].mediaDevices
    if js.isUndefined(devices) || devices == null || js.isUndefined(devices.enumerateDevices) then
      Future.successful(Seq.empty)
    else
      devices
        .enumerateDevices()
        .asInstanceOf[js.Promise[js.Array[js.Dynamic]]]
        .toFuture
        .map: found =>
          found.toSeq
            .filter(device => device.kind.asInstanceOf[String] == "videoinput")
            .map(device => VideoInput(device.deviceId.asInstanceOf[String], device.label.asInstanceOf[String]))
            .filter(_.deviceId.nonEmpty)
        .recover { case _ => Seq.empty }

  def preparePreview(element: dom.HTMLVideoElement): Unit =
    element.autoplay = true
    element.muted = true
    element.setAttribute("playsinline", "")
    element.setAttribute("webkit-playsinline", "")

  def attach(element: dom.HTMLVideoElement, stream: dom.MediaStream): Unit =
    element.srcObject = stream

  /** `resize` on a video element is newer than the scalajs-dom facade bundled with the application. */
  def onShapeChanged(element: dom.HTMLVideoElement)(listener: dom.Event => Unit): Unit =
    val callback: js.Function1[dom.Event, Unit] = listener
    element.asInstanceOf[js.Dynamic].onloadedmetadata = callback
    element.asInstanceOf[js.Dynamic].onresize = callback

  def detach(element: dom.HTMLVideoElement): Unit =
    element.srcObject = null

  def exceptionName(error: Throwable): String =
    error match
      case js.JavaScriptException(value) if value != null && !js.isUndefined(value) =>
        val name = value.asInstanceOf[js.Dynamic].name
        Option(name).filterNot(js.isUndefined).map(_.toString).getOrElse("")
      case _ => ""
