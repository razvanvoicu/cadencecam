package sgrv.fe.refreshstate

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import zio.json.*

import scala.util.control.NonFatal

@jsonNoExtraFields
private[fe] final case class RefreshState(
    active: Boolean,
    generation: Int,
    nextAttemptAtMillis: Option[Double],
    expired: Boolean
):
  def prepareForStartup: RefreshState =
    copy(active = false, generation = generation + 1, nextAttemptAtMillis = None, expired = false)

private[fe] object RefreshState:
  given JsonCodec[RefreshState] = DeriveJsonCodec.gen[RefreshState]

  val Inactive: RefreshState =
    RefreshState(active = false, generation = 0, nextAttemptAtMillis = None, expired = false)

/** Durable session-refresh state, deliberately independent of application-specific frontend state. */
private[fe] final class RefreshStateStore private (storage: dom.Storage, initialState: RefreshState):
  private val state = Var(initialState)

  def current: RefreshState = RefreshStateStore.load(storage)

  def signal: Signal[RefreshState] = state.signal

  def update(transform: RefreshState => RefreshState): Unit =
    val next = transform(current)
    RefreshStateStore.save(storage, next)
    state.set(next)

private[fe] object RefreshStateStore:
  private val StorageKey = "sgrv.refresh-state.v1"

  def apply(storage: dom.Storage): RefreshStateStore =
    val initialState = load(storage).prepareForStartup
    save(storage, initialState)
    new RefreshStateStore(storage, initialState)

  private def load(storage: dom.Storage): RefreshState =
    try
      Option(storage.getItem(StorageKey))
        .flatMap: encoded =>
          encoded.fromJson[RefreshState] match
            case Right(state) => Some(state)
            case Left(details) =>
              dom.console.warn(s"Ignoring invalid persisted refresh state: $details")
              None
        .getOrElse(RefreshState.Inactive)
    catch
      case NonFatal(error) =>
        dom.console.warn(s"Could not read persisted refresh state: ${message(error)}")
        RefreshState.Inactive

  private def save(storage: dom.Storage, state: RefreshState): Unit =
    try storage.setItem(StorageKey, state.toJson)
    catch
      case NonFatal(error) =>
        dom.console.warn(s"Could not persist refresh state: ${message(error)}")

  private def message(error: Throwable): String =
    Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
