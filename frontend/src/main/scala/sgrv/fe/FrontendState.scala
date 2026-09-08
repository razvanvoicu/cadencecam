package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.AboutInfo
import zio.json.*

import scala.util.control.NonFatal

private[fe] enum UserState:
  case Unknown
  case Unauthenticated

  /** Believed signed in from persisted state, but not yet confirmed by `/me`. Lets the last view reappear on reload
    * instead of a blank page, while still treating the backend as the only authority on whether the session is real.
    */
  case Restoring(email: String, displayName: String)
  case SignedIn(email: String, displayName: String)
  case AuthenticationFailed(message: String)

private[fe] object UserState:
  given JsonCodec[UserState] = DeriveJsonCodec.gen[UserState]

/** Matches the states that should render the application shell: confirmed, or optimistically restored. */
private[fe] object Present:
  def unapply(state: UserState): Option[String] = state match
    case UserState.SignedIn(_, displayName)  => Some(displayName)
    case UserState.Restoring(_, displayName) => Some(displayName)
    case _                                   => None

/** The role this device plays in a session. A signed-in device starts at [[Screen.Selection]] and stays on whichever
  * role it was given until the user goes back, so reopening the app on the capture phone returns it to capturing.
  */
private[fe] enum Screen:
  case Selection
  case Acquirer
  case Dashboard

private[fe] object Screen:
  given JsonCodec[Screen] = DeriveJsonCodec.gen[Screen]

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
    screen: Screen,
    /** The counting session this login belongs to, as reported by `/me`. Persisted so a reload still knows which record
      * it is working against before `/me` has answered again.
      */
    countingSessionId: Option[String],
    aboutState: AboutState,
    logoutState: LogoutState
):
  /** Authentication is always re-established from `/me` after a page load and transient UI operations are reset. The
    * selected screen survives, since it is a deliberate choice about this device rather than an in-flight operation.
    *
    * A previously signed-in user becomes [[UserState.Restoring]] rather than [[UserState.Unknown]], so the last view is
    * painted immediately. That is presentation only: no authenticated request is issued and no session renewal is
    * started until `/me` confirms the HttpOnly cookie, and a `401` replaces the view with the login page.
    */
  def prepareForStartup: FrontendState =
    copy(
      user = user match
        case UserState.SignedIn(email, displayName)  => UserState.Restoring(email, displayName)
        case UserState.Restoring(email, displayName) => UserState.Restoring(email, displayName)
        case _                                       => UserState.Unknown
      ,
      aboutState = AboutState.Closed,
      logoutState = LogoutState.Idle
    )

private[fe] object FrontendState:
  given JsonCodec[FrontendState] = DeriveJsonCodec.gen[FrontendState]

  val Initial: FrontendState = FrontendState(
    user = UserState.Unknown,
    screen = Screen.Selection,
    countingSessionId = None,
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
  private val StorageKey = "sgrv.frontend-state.v3"

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
            case Right(state)  => Some(state)
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
