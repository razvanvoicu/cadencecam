package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.AboutInfo
import sgrv.api.CountingSession
import sgrv.api.CurrentUser
import sgrv.fe.refreshstate.RefreshStateStore
import sgrv.fe.refreshstate.SessionRefreshWorker
import zio.json.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.Thenable.Implicits.*
import scala.util.Failure
import scala.util.Success

object Main:
  import UserState.*

  def main(args: Array[String]): Unit =
    val localStorage = dom.window.localStorage
    given stateStore: FrontendStateStore = FrontendStateStore(localStorage)
    given refreshStateStore: RefreshStateStore = RefreshStateStore(localStorage)

    /** Losing the session also abandons whichever role this device had taken. */
    def signedOut(state: UserState)(current: FrontendState): FrontendState =
      current.copy(
        user = state,
        screen = Screen.Selection,
        countingSessionId = None,
        aboutState = AboutState.Closed,
        logoutState = LogoutState.Idle
      )

    def updateUser(state: UserState): Unit =
      stateStore.update: current =>
        state match
          case SignedIn(_, _) => current.copy(user = state)
          case _              => signedOut(state)(current)

    def handleUnauthorized(): Unit =
      if worker.isEnabled then
        worker.disable()
        stateStore.update(signedOut(Unauthenticated))
        refreshStateStore.update(_.copy(expired = true))

    lazy val http: HttpService = HttpService(() => handleUnauthorized())
    lazy val worker: SessionRefreshWorker = SessionRefreshWorker(http)

    val initialSession = http.get("/me").flatMap(sessionState)
    initialSession.onComplete:
      case Success(MeResult(session @ SignedIn(email, _), countingSessionId)) =>
        stateStore.update: current =>
          current.copy(
            user = session,
            // A different account signing in on this device starts at the role picker rather than inheriting
            // whichever role the previous account left behind.
            screen = if stateStore.restoredUserEmail.contains(email) then current.screen else Screen.Selection,
            countingSessionId = countingSessionId
          )
        refreshStateStore.update(_.copy(expired = false))
        worker.enable()
      case Success(MeResult(session, _)) => updateUser(session)
      case Failure(error)                => updateUser(AuthenticationFailed(errorMessage(error)))

    def show(screen: Screen): Unit = stateStore.update(_.copy(screen = screen))

    def openAbout(): Unit =
      stateStore.update(_.copy(aboutState = AboutState.Loading))
      fetchAbout(http).onComplete:
        case Success(information) if stateStore.current.aboutState == AboutState.Loading =>
          stateStore.update(_.copy(aboutState = AboutState.Loaded(information)))
        case Failure(error) if stateStore.current.aboutState == AboutState.Loading =>
          val message = Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse("The request failed.")
          stateStore.update(_.copy(aboutState = AboutState.Failed(message)))
        case _ => ()

    def logout(): Unit =
      if stateStore.current.logoutState != LogoutState.InProgress then
        worker.disable()
        refreshStateStore.update(_.copy(expired = false))
        stateStore.update(_.copy(logoutState = LogoutState.InProgress))
        requestLogout(http).onComplete:
          case Success(_) =>
            stateStore.update(signedOut(Unauthenticated))
            dom.window.location.assign("/")
          case Failure(error) =>
            val message = Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse("The request failed.")
            stateStore.update(_.copy(logoutState = LogoutState.Failed(message)))

    def userActions: Element =
      div(
        cls := "user-actions",
        a(
          cls := "about-link",
          href := "/about",
          onClick.preventDefault --> (_ => openAbout()),
          "About"
        ),
        button(
          cls := "logout-link",
          typ := "button",
          disabled <-- stateStore.signal.map(_.logoutState == LogoutState.InProgress),
          child.text <-- stateStore.signal
            .map(_.logoutState)
            .map:
              case LogoutState.InProgress => "Logging out…"
              case _                      => "Logout",
          onClick --> (_ => logout())
        )
      )

    def roleChoice(modifier: String, screen: Screen, title: String, description: String): Element =
      button(
        cls := s"mode-button $modifier",
        typ := "button",
        onClick --> (_ => show(screen)),
        span(cls := "mode-title", title),
        span(cls := "mode-description", description)
      )

    def selection(displayName: String): Element =
      div(
        cls := "selection",
        h1(cls := "welcome", s"Hello, $displayName!"),
        p(cls := "selection-prompt", "Choose what this device does in the next session."),
        div(
          cls := "mode-choices",
          roleChoice(
            "mode-acquirer",
            Screen.Acquirer,
            "Signal acquirer",
            "Aim this device's camera at the movement and let it count the reps."
          ),
          roleChoice(
            "mode-dashboard",
            Screen.Dashboard,
            "Dashboard",
            "Watch the live rep count arriving from the acquiring device."
          )
        )
      )

    def signedInView(displayName: String, screen: Screen): Element =
      screen match
        case Screen.Selection => selection(displayName)
        case Screen.Acquirer  =>
          placeholder("acquirer", "Signal acquirer", "Camera capture and rep detection are not implemented yet.")
        case Screen.Dashboard =>
          placeholder(
            "dashboard",
            "Dashboard",
            "The live rep count from the acquiring device is not implemented yet."
          )

    def placeholder(modifier: String, heading: String, note: String): Element =
      div(
        cls := s"screen $modifier",
        h1(cls := "screen-title", heading),
        p(cls := "screen-note", note),
        button(
          cls := "back-button",
          typ := "button",
          "Back to selection",
          onClick --> (_ => show(Screen.Selection))
        )
      )

    val app =
      div(
        cls := "app",
        child <-- stateStore.signal
          .map(_.user)
          .map:
            case Present(_) => userActions
            case _          => emptyNode,
        div(
          cls := "content",
          child <-- stateStore.signal
            .map(state => (state.user, state.screen))
            .map:
              case (Unknown, _)         => emptyNode
              case (Unauthenticated, _) =>
                // Before authentication has been attempted, offer the only thing an anonymous visitor can do.
                div(cls := "home", a(cls := "login-button", href := "/auth/login", "Login with Google"))
              case (AuthenticationFailed(message), _) =>
                div(cls := "home", p(cls := "error", s"Authentication failed: $message"))
              // Confirmed and optimistically restored render identically; only the machinery around them differs.
              case (SignedIn(_, displayName), screen)  => signedInView(displayName, screen)
              case (Restoring(_, displayName), screen) => signedInView(displayName, screen)
        ),
        child <-- stateStore.signal
          .map(_.logoutState)
          .map:
            case LogoutState.Failed(message) => p(cls := "error logout-error", s"Logout failed: $message")
            case _                           => emptyNode,
        child <-- stateStore.signal
          .map(_.aboutState)
          .map:
            case AboutState.Closed => emptyNode
            case state             =>
              val content = state match
                case AboutState.Loading             => p(cls := "about-status", "Loading build information…")
                case AboutState.Failed(message)     => p(cls := "error about-status", message)
                case AboutState.Loaded(information) =>
                  dl(
                    cls := "about-details",
                    dt("App version"),
                    dd(information.appVersion),
                    dt("Build date"),
                    dd(information.buildDate),
                    dt("Build OS"),
                    dd(information.buildOs),
                    dt("Scala version"),
                    dd(information.scalaVersion),
                    dt("Scala.js version"),
                    dd(information.scalaJsVersion)
                  )
                case AboutState.Closed => emptyNode

              div(
                cls := "about-overlay",
                onClick --> (_ => stateStore.update(_.copy(aboutState = AboutState.Closed))),
                div(
                  cls := "about-dialog",
                  role := "dialog",
                  onClick --> (_.stopPropagation()),
                  div(
                    cls := "about-header",
                    h2("About"),
                    button(
                      cls := "about-close",
                      typ := "button",
                      title := "Close",
                      onClick --> (_ => stateStore.update(_.copy(aboutState = AboutState.Closed))),
                      "×"
                    )
                  ),
                  content
                )
              )
        ,
        child <-- refreshStateStore.signal
          .map(_.expired)
          .map:
            case false => emptyNode
            case true  =>
              div(
                cls := "about-overlay session-expired-overlay",
                div(
                  cls := "about-dialog session-expired-dialog",
                  role := "alertdialog",
                  div(
                    cls := "about-header",
                    h2("Session expired"),
                    button(
                      cls := "about-close",
                      typ := "button",
                      title := "Close",
                      onClick --> (_ => refreshStateStore.update(_.copy(expired = false))),
                      "×"
                    )
                  ),
                  p(
                    cls := "error session-expired-message",
                    "Your Google OAuth session has expired. Reload the page to sign in again."
                  )
                )
              )
      )

    renderOnDomContentLoaded(dom.document.body, app)

  /** What `/me` told us: who is signed in, and whichever counting session the backend filed alongside them. */
  private[fe] final case class MeResult(user: UserState, countingSessionId: Option[String])

  private def parseUser(
      json: String
  ): MeResult = // Extract the user's name from the Google account. Default to the email address if the name is not available.
    json
      .fromJson[CurrentUser]
      .fold(
        details => MeResult(AuthenticationFailed(s"The backend returned invalid user JSON: $details"), None),
        currentUser =>
          val user = Option(currentUser.email)
            .map(_.trim)
            .filter(_.nonEmpty)
            .map: address =>
              SignedIn(address, Option(currentUser.name).map(_.trim).filter(_.nonEmpty).getOrElse(address))
            .getOrElse(AuthenticationFailed("The backend returned no email address."))
          // An unreadable or absent entry is not an authentication problem, so it never downgrades the user state.
          val countingSessionId = currentUser.extra
            .getOrElse(Map.empty)
            .get(CountingSession.Key)
            .flatMap(_.as[CountingSession].toOption)
            .map(_.sessionId)
            .filter(_.nonEmpty)
          MeResult(user, countingSessionId)
      )

  private def sessionState(response: dom.Response): Future[MeResult] =
    if response.ok then response.text().map(parseUser)
    else if response.status == 401 then Future.successful(MeResult(Unauthenticated, None))
    else
      Future.successful(MeResult(AuthenticationFailed(s"The authentication check returned ${response.status}."), None))

  private def errorMessage(error: Throwable): String =
    Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse("The request failed.")

  private def requestLogout(http: HttpService): Future[Unit] =
    val init = new dom.RequestInit:
      method = dom.HttpMethod.POST
    http
      .send("/logout", init)
      .flatMap: response =>
        if response.ok then Future.successful(())
        else
          response
            .text()
            .flatMap: text =>
              val details = Option(text).map(_.trim).filter(_.nonEmpty).getOrElse(s"HTTP ${response.status}")
              Future.failed(RuntimeException(details))

  private def fetchAbout(http: HttpService): Future[AboutInfo] =
    http
      .get("/about")
      .flatMap: response =>
        if response.ok then
          response
            .text()
            .flatMap: text =>
              text
                .fromJson[AboutInfo]
                .fold(
                  details => Future.failed(RuntimeException(s"The backend returned invalid About JSON: $details")),
                  Future.successful
                )
        else Future.failed(RuntimeException(s"The About request returned ${response.status}."))
