package sgrv.fe.acquire

import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js

/** One camera setting the person holding the phone can move by hand, and the range it may move over. */
private[fe] final case class Adjustable(
    setting: String,
    label: String,
    mode: Option[String],
    min: Double,
    max: Double,
    step: Double,
    current: Double
)

/** Turning the camera's own numbers into something with a slider on it.
  *
  * Automatic metering is the problem this exists to escape, and holding the controls at whatever automatic last chose
  * only escapes the drift, not the choice. On a phone pointed at a bright screen in a dark room, what automatic
  * chooses is wrong from the start -- so the value has to be reachable by hand.
  */
private[fe] object CameraAdjust:

  /** What each setting is called where a person can see it, and the order they are offered in. */
  private[acquire] val offered: Seq[(String, String)] = Seq(
    "exposureTime" -> "Exposure",
    "iso" -> "Sensitivity",
    "colorTemperature" -> "Colour",
    "focusDistance" -> "Focus"
  )

  /** Which mode has to go manual before a setting can be moved, if any. */
  private[acquire] def governing(setting: String): Option[String] =
    Camera.manualControls.collectFirst { case control if control.settings.contains(setting) => control.mode }

  /** The settings this camera reports as a range, at the values it is presently using.
    *
    * A range is the test. Cameras report some capabilities as a list of modes and others as min/max/step, and only the
    * second kind has anywhere to put a slider. Anything reported without a usable span is left out rather than shown
    * as a control that cannot move.
    */
  private[fe] def adjustable(capabilities: js.Dynamic, settings: js.Dynamic): Seq[Adjustable] =
    if js.isUndefined(capabilities) || capabilities == null then Seq.empty
    else
      offered.flatMap: (setting, label) =>
        val span = capabilities.selectDynamic(setting)
        val now = settings.selectDynamic(setting)
        for
          range <- Option.when(!js.isUndefined(span) && span != null)(span)
          min <- number(range.selectDynamic("min"))
          max <- number(range.selectDynamic("max"))
          if max > min
          current <- number(now).orElse(Some(min))
        yield
          // A step the browser did not give is derived rather than assumed to be one: an exposure measured in
          // hundreds of microseconds and a colour temperature measured in kelvin cannot share a granularity.
          val step = number(range.selectDynamic("step")).filter(_ > 0).getOrElse((max - min) / 100.0)
          Adjustable(setting, label, governing(setting), min, max, step, current.max(min).min(max))

  /** A number the camera reported, or nothing.
    *
    * Checked by JavaScript's own idea of the type rather than by a cast. A capability the browser reports as a string,
    * an object or not at all would otherwise become a `Double` that is quietly NaN, and a slider with NaN for a bound
    * renders but cannot be moved.
    */
  private def number(value: js.Dynamic): Option[Double] =
    Option
      .when(!js.isUndefined(value) && value != null && js.typeOf(value) == "number")(value.asInstanceOf[Double])
      .filterNot(_.isNaN)

  /** Moves one setting, switching its mode off automatic first when it has one.
    *
    * Two requests again, and for the same reason the held controls need two: a camera still deciding for itself throws
    * away a value that arrives alongside the instruction to stop.
    */
  private[fe] def move(stream: dom.MediaStream, adjustable: Adjustable, value: Double): Future[Unit] =
    stream.getVideoTracks().headOption match
      case None => Future.successful(())
      case Some(track) =>
        def request(wanted: js.Dynamic): Future[Unit] =
          track
            .applyConstraints(js.Dynamic.literal(advanced = js.Array(wanted)).asInstanceOf[dom.MediaTrackConstraints])
            .toFuture
            .map(_ => ())
        val setting = js.Dynamic.literal()
        setting.updateDynamic(adjustable.setting)(value)
        adjustable.mode match
          case None => request(setting)
          case Some(mode) =>
            val manual = js.Dynamic.literal()
            manual.updateDynamic(mode)("manual")
            request(manual).flatMap(_ => request(setting))

  /** What the camera says it is doing now, for the sliders to be re-seated from after anything is applied. */
  private[fe] def settingsOf(stream: dom.MediaStream): Option[js.Dynamic] =
    stream
      .getVideoTracks()
      .headOption
      .map(_.asInstanceOf[js.Dynamic])
      .filterNot(track => js.isUndefined(track.getSettings))
      .map(_.getSettings())

  private[fe] def capabilitiesOf(stream: dom.MediaStream): Option[js.Dynamic] =
    stream
      .getVideoTracks()
      .headOption
      .map(_.asInstanceOf[js.Dynamic])
      .filterNot(track => js.isUndefined(track.getCapabilities))
      .map(_.getCapabilities())
