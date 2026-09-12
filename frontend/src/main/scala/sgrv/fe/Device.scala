package sgrv.fe

import org.scalajs.dom
import scala.scalajs.js

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

  /** The hints a browser offers, preferred in the order they are specific.
    *
    * `userAgentData` is the modern, structured form and gives the platform cleanly; the user agent string carries the
    * model on Android. Both are taken when both exist, since neither alone has been enough.
    */
  def describe(): Option[String] =
    val navigator = dom.window.navigator.asInstanceOf[js.Dynamic]
    val parts = Seq(fromUserAgentData(navigator), model(text(navigator.userAgent)), text(navigator.platform))
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
