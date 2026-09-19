package sgrv.fe.acquire

import org.scalajs.dom
import sgrv.api.RepProgress
import sgrv.fe.HttpService
import zio.json.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.Failure
import scala.util.Success

/** Reports what the acquirer's workout has measured so far to the account's session, at a fixed interval.
  *
  * The whole measurement rather than the count alone: the rep count, the cadence accumulated over it, and how long the
  * set has run. The history works its calorie figure and its duration out of the last two, and a session that was only
  * ever told the count shows every workout as having taken no time and, counted by frequency, burned nothing.
  *
  * On a timer rather than on every rep: the session needs a record that is roughly current, not every increment, and a
  * workout at two hertz would otherwise mean a request per half second. Reporting unchanged totals too is deliberate —
  * the traffic is also what keeps a scale-to-zero backend from being reclaimed while an acquirer sits mid-workout,
  * which a report only on change would not do during a rest.
  *
  * Jitter is acceptable and no attempt is made to correct for it: nothing downstream measures cadence from when these
  * arrive, and the backend stamps each report with its own clock.
  */
private[fe] final class RepProgressReporter(
    http: HttpService,
    intervalMillis: Int = RepProgressReporter.DefaultIntervalMillis
):
  private var handle: Option[Int] = None
  private var inFlight = false

  /** Called when the server says another device now holds the counter's role.
    *
    * Nothing else carries a stand-down, so this is how a displaced device learns: its next report is refused with a
    * conflict, because the account's record names somebody else as the counter.
    */
  var onDisplaced: () => Unit = () => ()

  /** Begins reporting whatever `progress` returns at each tick. Starting twice is a no-op rather than a second timer.
    */
  def start(progress: () => RepProgress): Unit =
    if handle.isEmpty then handle = Some(dom.window.setInterval(() => report(progress()), intervalMillis.toDouble))

  def stop(): Unit =
    handle.foreach(dom.window.clearInterval)
    handle = None

  private def report(progress: RepProgress): Unit =
    // A slow network must not let reports queue up behind each other: the next tick carries the same information as
    // the one still in flight, so skipping it loses nothing.
    if !inFlight then
      inFlight = true
      val init = new dom.RequestInit:
        method = dom.HttpMethod.POST
        headers = js.Dictionary("Content-Type" -> "application/json")
        body = progress.toJson
      http
        .send(RepProgress.Path, init)
        .onComplete: outcome =>
          inFlight = false
          outcome match
            // Failure is not escalated and the timer keeps running: the next tick reports the same total, so a lost
            // report costs nothing. A 401 is already handled by the HTTP boundary, which ends the session.
            case Success(response) if response.status == 409 =>
              dom.console.info("Another device is now counting for this account; standing down")
              stop()
              onDisplaced()
            case Success(response) if !response.ok && response.status != 401 =>
              dom.console.warn(s"Reporting the rep count returned HTTP ${response.status}")
            case Failure(error) =>
              dom.console.warn(s"Could not report the rep count: ${RepProgressReporter.message(error)}")
            case Success(_) => ()

private[fe] object RepProgressReporter:
  /** Frequent enough to keep the backend warm and the session's record close to current, without making it hot. */
  val DefaultIntervalMillis: Int = 10 * 1000

  private def message(error: Throwable): String =
    Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
