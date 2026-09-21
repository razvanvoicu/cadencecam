package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.fe.ApiClient.{ApiError, MeResult}
import sgrv.fe.UserState.*
import sgrv.fe.refreshstate.RefreshStateStore
import sgrv.fe.refreshstate.SessionRefreshWorker

import scala.concurrent.ExecutionContext.Implicits.global

/** Owns authentication and session lifecycle.
  *
  * The controller is deliberately independent of the rendered screen. It changes durable application state and calls
  * the two pieces of signed-in startup work supplied by the composition root; the view merely observes those changes.
  */
private[fe] final class SessionController(
    api: ApiClient,
    worker: SessionRefreshWorker,
    stateStore: FrontendStateStore,
    refreshStateStore: RefreshStateStore,
    storage: dom.Storage,
    onSignedIn: () => Unit,
    startByPresence: () => Unit
):
  /** Raised while the account is being asked whether it meant it. Deleting everything is the one thing in the app that
    * cannot be undone from inside the app, so it is asked for twice and said plainly the second time.
    */
  val forgetPending: Var[Boolean] = Var(false)
  private val aboutRequests = RequestScope()

  def start(): Unit =
    api
      .session()
      .foreach:
        case Right(MeResult(session @ SignedIn(email, _), countingSessionId)) =>
          // What this device has been told to open on, if anything. Read once, here: the one-shot marker it may consume
          // belongs to this start and no other.
          val preferred = StartPreference.consumeOpening(storage)
          stateStore.update: current =>
            current.copy(
              user = session,
              // A preference wins. Otherwise a different account signing in on this device starts at the role picker
              // rather than inheriting whichever role the previous account left behind.
              screen = preferred.getOrElse:
                if stateStore.restoredUserEmail.contains(email) then current.screen else Screen.Selection
              ,
              countingSessionId = countingSessionId
            )
          refreshStateStore.update(_.copy(expired = false))
          worker.enable()
          onSignedIn()
          // Only for a login that has just landed on the picker with nothing preferred: a device returning to the role it
          // already held keeps it, and must not be sent to the camera because some other device happens to be free.
          if preferred.isEmpty && !stateStore.restoredUserEmail.contains(email) then startByPresence()
        case Right(MeResult(session, _)) => updateUser(session)
        case Left(error)                 => updateUser(AuthenticationFailed(error.message))

  def handleUnauthorized(): Unit =
    // A session revoked by Logout is indistinguishable from one that expired, and should not be: both mean this device
    // is signed out. Move straight to the Google login view instead of leaving an expiry dialog over an old screen.
    aboutRequests.invalidate()
    worker.disable()
    stateStore.update(SessionController.signedOut(Unauthenticated))
    refreshStateStore.update(_.copy(expired = false))

  def openAbout(): Unit =
    val signedIn = stateStore.current.user match
      case SignedIn(_, _) | Restoring(_, _) => true
      case _                                => false
    // Nobody to ask before signing in: the build endpoint needs a session, so asking only ever produced a 401 dressed
    // up as a failure, and tripped the session handling on the way out. What is worth knowing there -- which bundle
    // this browser is running -- is known locally.
    if !signedIn then
      aboutRequests.invalidate()
      stateStore.update(_.copy(aboutState = AboutState.LocalOnly))
    else
      stateStore.update(_.copy(aboutState = AboutState.Loading))
      aboutRequests.latest(api.about()):
        case Right(information) if stateStore.current.aboutState == AboutState.Loading =>
          stateStore.update(_.copy(aboutState = AboutState.Loaded(information)))
        case Left(error) if stateStore.current.aboutState == AboutState.Loading =>
          stateStore.update(_.copy(aboutState = AboutState.Failed(error.message)))
        case _ => ()

  def logout(): Unit =
    if stateStore.current.logoutState != LogoutState.InProgress then
      worker.disable()
      refreshStateStore.update(_.copy(expired = false))
      stateStore.update(_.copy(logoutState = LogoutState.InProgress))
      api
        .logout()
        .foreach:
          case Right(_) =>
            stateStore.update(SessionController.signedOut(Unauthenticated))
          // The HTTP boundary has already moved a remotely revoked device to Login. Reporting that same 401 as a
          // failed logout would put contradictory wording on the login screen.
          case Left(ApiError.Http(_, 401, _)) if stateStore.current.user == Unauthenticated => ()
          case Left(error) =>
            worker.enable()
            stateStore.update(_.copy(logoutState = LogoutState.Failed(error.message)))

  /** Removes everything the account holds, then signs out.
    *
    * Signing out afterwards because what is left otherwise is a session pointing at an account that no longer exists:
    * every screen would be reading records that had just been deleted and quietly showing their absence as zeroes.
    * Ending the session is the honest state to be in when nothing is left.
    */
  def forgetEverything(): Unit =
    forgetPending.set(false)
    api
      .deleteAccountData()
      .foreach:
        case Right(_)               => logout()
        case Left(_: ApiError.Http) =>
          stateStore.update(_.copy(logoutState = LogoutState.Failed("Your data could not be deleted.")))
        case Left(error) =>
          stateStore.update(_.copy(logoutState = LogoutState.Failed(error.message)))

  private def updateUser(state: UserState): Unit =
    stateStore.update: current =>
      state match
        case SignedIn(_, _) => current.copy(user = state)
        case _              => SessionController.signedOut(state)(current)

private[fe] object SessionController:
  /** Losing the session also abandons whichever role this device had taken. Kept pure so lifecycle tests need neither
    * browser storage nor an HTTP server.
    */
  def signedOut(state: UserState)(current: FrontendState): FrontendState =
    current.copy(
      user = state,
      screen = Screen.Selection,
      countingSessionId = None,
      aboutState = AboutState.Closed,
      logoutState = LogoutState.Idle
    )
