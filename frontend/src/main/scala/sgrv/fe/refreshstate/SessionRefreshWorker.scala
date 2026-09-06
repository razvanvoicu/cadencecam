package sgrv.fe.refreshstate

import org.scalajs.dom
import sgrv.fe.HttpService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.Failure
import scala.util.Success

/** Keeps an authenticated browser session alive without depending on application-specific frontend state. */
private[fe] final class SessionRefreshWorker(
    http: HttpService,
    refreshLeadTimeMillis: Int = SessionRefreshWorker.DefaultRefreshLeadTimeMillis,
    retryDelayMillis: Int = SessionRefreshWorker.DefaultRetryDelayMillis
)(using stateStore: RefreshStateStore):
  def isEnabled: Boolean = readState.active

  def enable(): Unit =
    val current = readState
    if !current.active then
      val generation = current.generation + 1
      writeState(current.copy(active = true, generation = generation, nextAttemptAtMillis = None))
      renew(generation)

  def disable(): Unit =
    val current = readState
    writeState(current.copy(active = false, generation = current.generation + 1, nextAttemptAtMillis = None))

  private def renew(generation: Int): Unit =
    if isCurrent(generation) then
      writeState(readState.copy(nextAttemptAtMillis = None))
      val init = new dom.RequestInit:
        method = dom.HttpMethod.POST
      http
        .send("/refreshSession", init)
        .map: response =>
          if response.ok then
            val expiresAt = Option(response.headers.get(SessionRefreshWorker.ExpiresAtHeader))
              .map(js.Date.parse)
              .filterNot(_.isNaN)
              .getOrElse(throw IllegalStateException("The session renewal response carries no valid expiry"))
            schedule(generation, SessionRefreshWorker.nextDelayMillis(js.Date.now(), expiresAt, refreshLeadTimeMillis))
          else if response.status != 401 then
            throw IllegalStateException(s"Session renewal returned HTTP ${response.status}")
        .onComplete:
          case Failure(error) if isCurrent(generation) =>
            dom.console.warn(s"Session renewal failed; retrying: ${SessionRefreshWorker.message(error)}")
            schedule(generation, retryDelayMillis)
          case Success(_) | Failure(_) => ()

  private def schedule(generation: Int, delayMillis: Int): Unit =
    if isCurrent(generation) then
      writeState(readState.copy(nextAttemptAtMillis = Some(js.Date.now() + delayMillis)))
      val _ = dom.window.setTimeout(() => renew(generation), delayMillis)

  private def isCurrent(generation: Int): Boolean =
    val current = readState
    current.active && current.generation == generation

  private def readState: RefreshState = stateStore.current

  private def writeState(state: RefreshState): Unit =
    stateStore.update(_ => state)

private[fe] object SessionRefreshWorker:
  val ExpiresAtHeader = "X-Session-Expires-At"
  val DefaultRefreshLeadTimeMillis: Int = 5 * 60 * 1000
  val DefaultRetryDelayMillis: Int = 60 * 1000
  private val MinimumDelayMillis = 1000

  private[fe] def nextDelayMillis(nowMillis: Double, expiresAtMillis: Double, leadTimeMillis: Int): Int =
    val requested = expiresAtMillis - nowMillis - leadTimeMillis
    math.max(MinimumDelayMillis.toDouble, math.min(requested, Int.MaxValue.toDouble)).toInt

  private def message(error: Throwable): String =
    Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
