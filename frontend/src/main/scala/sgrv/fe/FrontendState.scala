package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.AboutInfo
import zio.json.*

import scala.util.control.NonFatal

private[fe] enum UserState:
  case Unknown
  case Unauthenticated
  case SignedIn(email: String, displayName: String)
  case AuthenticationFailed(message: String)

private[fe] object UserState:
  given JsonCodec[UserState] = DeriveJsonCodec.gen[UserState]

private[fe] enum SheetState:
  case Idle
  case Loading
  case Loaded(rows: Seq[Seq[String]])
  case Failed(message: String)

private[fe] object SheetState:
  given JsonCodec[SheetState] = DeriveJsonCodec.gen[SheetState]

private[fe] enum AboutState:
  case Closed
  case Loading
  case Loaded(information: AboutInfo)
  case Failed(message: String)

private[fe] object AboutState:
  given JsonCodec[AboutState] = DeriveJsonCodec.gen[AboutState]

private[fe] enum LogoutState:
  case Idle
  case InProgress
  case Failed(message: String)

private[fe] object LogoutState:
  given JsonCodec[LogoutState] = DeriveJsonCodec.gen[LogoutState]

@jsonNoExtraFields
private[fe] final case class FrontendState(
    user: UserState,
    sheetName: String,
    sheetState: SheetState,
    aboutState: AboutState,
    logoutState: LogoutState
):
  /** Authentication is always re-established from `/me` after a page load and transient UI operations are reset. */
  def prepareForStartup: FrontendState =
    copy(
      user = UserState.Unknown,
      sheetState = sheetState match
        case SheetState.Loading => SheetState.Idle
        case state              => state,
      aboutState = AboutState.Closed,
      logoutState = LogoutState.Idle
    )

private[fe] object FrontendState:
  given JsonCodec[FrontendState] = DeriveJsonCodec.gen[FrontendState]

  val Initial: FrontendState = FrontendState(
    user = UserState.Unknown,
    sheetName = "",
    sheetState = SheetState.Idle,
    aboutState = AboutState.Closed,
    logoutState = LogoutState.Idle
  )

/** Owns both durable browser persistence and the Laminar projection used to render it. */
private[fe] final class FrontendStateStore private (
    storage: dom.Storage,
    initialState: FrontendState,
    val restoredUserEmail: Option[String]
):
  private val state = Var(initialState)

  def current: FrontendState = FrontendStateStore.load(storage)

  def signal: Signal[FrontendState] = state.signal

  def update(transform: FrontendState => FrontendState): Unit =
    val next = transform(current)
    FrontendStateStore.save(storage, next)
    state.set(next)

private[fe] object FrontendStateStore:
  private val StorageKey = "sgrv.frontend-state.v2"

  def apply(storage: dom.Storage): FrontendStateStore =
    val restoredState = load(storage)
    val restoredUserEmail = restoredState.user match
      case UserState.SignedIn(email, _) => Some(email)
      case _                            => None
    val initialState = restoredState.prepareForStartup
    save(storage, initialState)
    new FrontendStateStore(storage, initialState, restoredUserEmail)

  private def load(storage: dom.Storage): FrontendState =
    try
      Option(storage.getItem(StorageKey))
        .flatMap: encoded =>
          encoded.fromJson[FrontendState] match
            case Right(state) => Some(state)
            case Left(details) =>
              dom.console.warn(s"Ignoring invalid persisted frontend state: $details")
              None
        .getOrElse(FrontendState.Initial)
    catch
      case NonFatal(error) =>
        dom.console.warn(s"Could not read persisted frontend state: ${message(error)}")
        FrontendState.Initial

  private def save(storage: dom.Storage, state: FrontendState): Unit =
    try storage.setItem(StorageKey, state.toJson)
    catch
      case NonFatal(error) =>
        dom.console.warn(s"Could not persist frontend state: ${message(error)}")

  private def message(error: Throwable): String =
    Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
