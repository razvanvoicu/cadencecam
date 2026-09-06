package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.AboutInfo
import sgrv.api.CurrentUser
import sgrv.api.SpreadsheetContentResponse
import sgrv.api.UpsertSpreadsheetRequest
import sgrv.fe.refreshstate.RefreshStateStore
import sgrv.fe.refreshstate.SessionRefreshWorker
import zio.json.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*
import scala.util.Failure
import scala.util.Success

object Main:
  import UserState.*
  import SheetState.*

  def main(args: Array[String]): Unit =
    val localStorage = dom.window.localStorage
    given stateStore: FrontendStateStore = FrontendStateStore(localStorage)
    given refreshStateStore: RefreshStateStore = RefreshStateStore(localStorage)

    def updateUser(state: UserState): Unit =
      stateStore.update: current =>
        state match
          case SignedIn(_, _) => current.copy(user = state)
          case _              =>
            current.copy(
              user = state,
              aboutState = AboutState.Closed,
              sheetState = Idle,
              logoutState = LogoutState.Idle
            )

    def handleUnauthorized(): Unit =
      if worker.isEnabled then
        worker.disable()
        stateStore.update: current =>
          current.copy(
            user = Unauthenticated,
            aboutState = AboutState.Closed,
            sheetState = Idle,
            logoutState = LogoutState.Idle
          )
        refreshStateStore.update(_.copy(expired = true))

    lazy val http: HttpService = HttpService(() => handleUnauthorized())
    lazy val worker: SessionRefreshWorker = SessionRefreshWorker(http)

    val initialSession = http.get("/me").flatMap(sessionState)
    initialSession.onComplete:
      case Success(session @ SignedIn(email, _)) =>
        stateStore.update: current =>
          current.copy(
            user = session,
            sheetState = if stateStore.restoredUserEmail.contains(email) then current.sheetState else Idle
          )
        refreshStateStore.update(_.copy(expired = false))
        worker.enable()
      case Success(session) => updateUser(session)
      case Failure(error)   => updateUser(AuthenticationFailed(errorMessage(error)))

    def upsertAndFetch(): Unit =
      val name = stateStore.current.sheetName.trim
      if name.isEmpty then stateStore.update(_.copy(sheetState = Failed("Enter a spreadsheet name first.")))
      else
        stateStore.update(_.copy(sheetState = Loading))
        postJson(http, "/sheets/upsert", UpsertSpreadsheetRequest(name))
          .flatMap(_ => fetchContent(http, name))
          .onComplete:
            case Success(rows)  => stateStore.update(_.copy(sheetState = Loaded(rows)))
            case Failure(error) => stateStore.update(_.copy(sheetState = Failed(error.getMessage)))

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
            stateStore.update: current =>
              current.copy(
                user = Unauthenticated,
                aboutState = AboutState.Closed,
                sheetState = Idle,
                logoutState = LogoutState.Idle
              )
            dom.window.location.assign("/")
          case Failure(error) =>
            val message = Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse("The request failed.")
            stateStore.update(_.copy(logoutState = LogoutState.Failed(message)))

    val app =
      div(
        cls := "app",
        child <-- stateStore.signal
          .map(_.user)
          .map:
            case SignedIn(_, _) =>
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
            case _ => emptyNode,
        div(
          cls := "home",
          child <-- stateStore.signal
            .map(_.user)
            .map:
              case Unknown         => emptyNode
              case Unauthenticated =>
                a(
                  cls := "login-button",
                  href := "/auth/login",
                  "Login with Google"
                ) // Before authentication has been attempted, disply a login option
              case SignedIn(_, displayName) =>
                h1(cls := "welcome", s"Hello, $displayName!") // After successfull authentication, show welcome message
              case AuthenticationFailed(message) =>
                p(cls := "error", s"Authentication failed: $message") // Error message if authentication fails
        ),
        child <-- stateStore.signal
          .map(_.logoutState)
          .map:
            case LogoutState.Failed(message) => p(cls := "error logout-error", s"Logout failed: $message")
            case _                           => emptyNode,
        child <-- stateStore.signal
          .map(_.user)
          .map:
            case SignedIn(_, _) =>
              div(
                div(
                  cls := "sheet-row",
                  span("Input the name of a sample spreadsheet:"),
                  input(
                    typ := "text",
                    placeholder := "Sample spreadsheet",
                    value <-- stateStore.signal.map(_.sheetName),
                    onInput.mapToValue --> (value => stateStore.update(_.copy(sheetName = value)))
                  ),
                  button(
                    typ := "button",
                    "Create or update spreadsheet",
                    onClick --> (_ => upsertAndFetch())
                  )
                ),
                child <-- stateStore.signal
                  .map(_.sheetState)
                  .map:
                    case Idle                         => emptyNode
                    case Loading                      => p("Working…")
                    case Failed(reason)               => p(cls := "error", reason)
                    case Loaded(rows) if rows.isEmpty => p("No rows yet.")
                    case Loaded(rows)                 =>
                      table(
                        cls := "sheet-table",
                        thead(tr(th("Timestamp"), th("User agent"))),
                        tbody(rows.map(row => tr(row.map(cell => td(cell)))))
                      )
              )
            case _ => emptyNode,
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

  private def parseUser(
      json: String
  ): UserState = // Extract the user's name from the Google account. Default to the email address if the name is not available.
    json
      .fromJson[CurrentUser]
      .fold(
        details => AuthenticationFailed(s"The backend returned invalid user JSON: $details"),
        currentUser =>
          Option(currentUser.email)
            .map(_.trim)
            .filter(_.nonEmpty)
            .map: address =>
              SignedIn(address, Option(currentUser.name).map(_.trim).filter(_.nonEmpty).getOrElse(address))
            .getOrElse(AuthenticationFailed("The backend returned no email address."))
      )

  private def sessionState(response: dom.Response): Future[UserState] =
    if response.ok then response.text().map(parseUser)
    else if response.status == 401 then Future.successful(Unauthenticated)
    else Future.successful(AuthenticationFailed(s"The authentication check returned ${response.status}."))

  private def errorMessage(error: Throwable): String =
    Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse("The request failed.")

  private def postJson[A: JsonEncoder](http: HttpService, url: String, value: A): Future[Unit] =
    val requestHeaders = dom.Headers()
    requestHeaders.set("Content-Type", "application/json")
    val init = new dom.RequestInit:
      method = dom.HttpMethod.POST
      headers = requestHeaders
      body = value.toJson
    http
      .send(url, init)
      .flatMap: response =>
        if response.ok then Future.successful(())
        else response.text().flatMap(text => Future.failed(RuntimeException(s"${response.status}: $text")))

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

  private def fetchContent(http: HttpService, name: String): Future[Seq[Seq[String]]] =
    http
      .get(s"/sheets/content?name=${js.URIUtils.encodeURIComponent(name)}")
      .flatMap: response =>
        if response.ok then
          response
            .text()
            .flatMap(text =>
              parseRows(text).fold(
                details => Future.failed(RuntimeException(s"The backend returned invalid spreadsheet JSON: $details")),
                rows => Future.successful(rows)
              )
            )
        else response.text().flatMap(text => Future.failed(RuntimeException(s"${response.status}: $text")))

  private def parseRows(json: String): Either[String, Seq[Seq[String]]] =
    json.fromJson[SpreadsheetContentResponse].map(_.rows)
