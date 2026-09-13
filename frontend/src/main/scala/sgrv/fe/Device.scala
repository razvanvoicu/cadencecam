package sgrv.fe

import org.scalajs.dom
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global

/** What this device is, as far as a browser will admit.
  *
  * Recorded on every test event because the answer to "why did that suite undercount" has depended on it more than on
  * anything in the detector: runs from one account on different handsets have come back exactly right, one short on
  * every test, and fifteen short. Telling those apart afterwards meant asking whoever ran them what was on the desk.
  *
  * How much can be learned varies by platform and there is nothing to be done about it. Android puts the model in the
  * user agent, so a phone names itself. Safari on iOS says only "iPhone" whatever the handset, deliberately, and no
  * amount of asking changes that -- which is worth knowing when reading the field rather than discovering later.
  */
private[fe] object Device:

  /** What the browser will only say when asked directly, once it has answered.
    *
    * Chrome on Android no longer puts the handset's model in its user agent: every device reports the frozen
    * placeholder "K", so four suites run on four different phones all filed themselves as "Android 10, K" and the field
    * was worth nothing for the one job it exists to do. The model is still available, but only through an explicit
    * request, and that request is asynchronous -- so it is made once at startup and the answer kept.
    */
  private var asked = Option.empty[String]

  /** Asks for the model the user agent omits. Chromium only; elsewhere there is nothing to ask and nothing is lost.
    *
    * Fire and forget: readings and recordings both happen seconds later at the earliest, and a device that answers late
    * simply describes itself from the next one onwards rather than blocking anything.
    */
  def learn(): Unit =
    val navigator = dom.window.navigator.asInstanceOf[js.Dynamic]
    val data = navigator.userAgentData
    if js.isUndefined(data) || data == null || js.isUndefined(data.getHighEntropyValues) then ()
    else
      try
        data
          .getHighEntropyValues(js.Array("model", "platformVersion"))
          .asInstanceOf[js.Promise[js.Dynamic]]
          .toFuture
          .foreach(values => asked = named(text(values.model), text(values.platformVersion)))
      catch case _: Throwable => ()

  /** The two answers as one phrase, or nothing when the browser named neither. */
  private[fe] def named(model: Option[String], platformVersion: Option[String]): Option[String] =
    val parts = Seq(model.filter(_.nonEmpty), platformVersion.filter(_.nonEmpty).map(version => s"v$version")).flatten
    Option.when(parts.nonEmpty)(parts.mkString(" "))

  /** The hints a browser offers, preferred in the order they are specific.
    *
    * `userAgentData` is the modern, structured form and gives the platform cleanly; the user agent string carries the
    * model on Android. Both are taken when both exist, since neither alone has been enough.
    */
  def describe(): Option[String] =
    val navigator = dom.window.navigator.asInstanceOf[js.Dynamic]
    val parts =
      Seq(asked, fromUserAgentData(navigator), model(text(navigator.userAgent)), text(navigator.platform))
    val described = parts.flatten.distinct.filter(_.nonEmpty)
    Option.when(described.nonEmpty)(described.mkString("; ").take(200))

  private def text(value: js.Dynamic): Option[String] =
    Option(value).filterNot(js.isUndefined).map(_.toString).map(_.trim).filter(_.nonEmpty)

  private def fromUserAgentData(navigator: js.Dynamic): Option[String] =
    val data = navigator.userAgentData
    if js.isUndefined(data) || data == null then None
    else
      val platform = text(data.platform)
      val mobile = Option(data.mobile).filterNot(js.isUndefined).map(_.toString)
      platform.map(name => mobile.filter(_ == "true").fold(name)(_ => s"$name mobile"))

  /** The model out of a user agent string, where there is one.
    *
    * Android encodes it between the build tag and the browser's own section, as in "Linux; Android 14; SM-A536B".
    * Anything else keeps the whole string, trimmed, because guessing at a shape that is not there loses more than it
    * tidies.
    */
  private[fe] def model(userAgent: Option[String]): Option[String] =
    userAgent.map: agent =>
      val inside = agent.dropWhile(_ != '(').drop(1).takeWhile(_ != ')')
      val fields = inside.split(';').map(_.trim).filter(_.nonEmpty)
      if fields.exists(_.startsWith("Android")) then fields.filterNot(_ == "Linux").mkString(", ")
      else if fields.nonEmpty then fields.mkString(", ")
      else agent

  /** The build this bundle was stamped with as it was packaged, if the browser kept it.
    *
    * Written by a line the build prepends to `main.js`, so it describes the code actually running rather than the code
    * the server currently serves. A browser holding a cached bundle looks exactly like one that has picked up the
    * latest, and several hours of test results have been read as detector behaviour when they were an old build on one
    * phone.
    */
  val BuildKey = "cadencecam.frontendBuild"

  def frontendBuild(): Option[String] =
    try Option(dom.window.localStorage.getItem(BuildKey)).map(_.trim).filter(_.nonEmpty)
    catch case _: Throwable => None
