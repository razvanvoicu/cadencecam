package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.AboutInfo
import sgrv.api.CountingSession
import sgrv.api.CurrentUser
import sgrv.fe.acquire.{
  Camera,
  CameraState,
  CommonMode,
  FrameSampler,
  Quadrant,
  QuadrantSignals,
  Sample,
  SignalGraph,
  SignalZoom
}
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
        case Screen.Acquirer  => acquirer()
        case Screen.Dashboard =>
          placeholder(
            "dashboard",
            "Dashboard",
            "The live rep count from the acquiring device is not implemented yet."
          )

    /** The camera and its sampler are browser resources, so they live outside the persisted FrontendState and are torn
      * down when this view unmounts — otherwise the camera would stay held after leaving the screen.
      */
    def acquirer(): Element =
      val cameraState = Var[CameraState](CameraState.Idle)
      val repCount = Var(0)
      val signals = QuadrantSignals()
      val tick = Var(0)
      var stream: Option[dom.MediaStream] = None
      var sampler: Option[FrameSampler] = None

      val video = videoTag(cls := "camera-video")
      // The overlay's lines sit at 50% of this box, so the box must be exactly the frame: its aspect ratio is set
      // from the stream once known, keeping the drawn quadrants aligned with the sampled ones.
      val frame = div(
        cls := "camera-frame",
        cls("mirrored") <-- stateStore.signal.map(_.mirrored),
        video,
        div(cls := "quadrant-lines")
      )

      // Set on the element rather than as Laminar attributes: playsinline in particular is what stops iOS taking
      // the preview fullscreen, and it has to be a real attribute on the node before play() is called.
      def prepare(element: dom.HTMLVideoElement): Unit =
        val media = element.asInstanceOf[js.Dynamic]
        media.autoplay = true
        media.muted = true
        element.setAttribute("playsinline", "")
        element.setAttribute("webkit-playsinline", "")

      // Measured rather than assumed: if the sampling loop stalls, the reading drops instead of the view claiming
      // a rate it is not achieving.
      val measuredHz = Var(Option.empty[Double])
      var firstSampleAt = Option.empty[Double]
      var sampleCount = 0

      def onSample(sample: Sample): Unit =
        signals.record(sample)
        tick.update(_ + 1)
        sampleCount += 1
        firstSampleAt match
          case None        => firstSampleAt = Some(sample.atMillis)
          case Some(start) =>
            val elapsed = sample.atMillis - start
            if elapsed > 0 then measuredHz.set(Some((sampleCount - 1) * 1000.0 / elapsed))

      def release(): Unit =
        sampler.foreach(_.stop())
        sampler = None
        stream.foreach(Camera.stop)
        stream = None
        firstSampleAt = None
        sampleCount = 0
        measuredHz.set(None)
        signals.clear()

      def startCamera(): Unit =
        if cameraState.now() != CameraState.Starting then
          cameraState.set(CameraState.Starting)
          Camera
            .start()
            .onComplete:
              case Success(opened) =>
                stream = Some(opened)
                val element = video.ref
                prepare(element)
                element.asInstanceOf[js.Dynamic].srcObject = opened.asInstanceOf[js.Any]
                val _ = element.play()
                val (width, height) = Camera.resolution(opened).getOrElse((0, 0))
                if width > 0 && height > 0 then frame.ref.style.setProperty("aspect-ratio", s"$width / $height")
                cameraState.set(CameraState.Streaming(width, height))
                val started = FrameSampler(element, onSample, () => stateStore.current.mirrored)
                started.start()
                sampler = Some(started)
              case Failure(error) =>
                release()
                cameraState.set(CameraState.Unavailable(Camera.failureMessage(error)))

      def graphPane(quadrant: Quadrant): Element =
        val pane = canvasTag(cls := "signal-canvas")
        div(
          cls := "signal-pane",
          span(cls := "signal-label", quadrant.toString),
          // Repeated on every pane rather than stated once: the control that sets it lives in the panel above and
          // scrolls out of sight exactly when the traces are being read.
          span(
            cls := "signal-scale",
            child.text <-- stateStore.signal.map(state => SignalZoom.label(state.signalZoom))
          ),
          pane,
          // Redraw on every sample, so the trace keeps scrolling on a still scene too: samples arrive whether or
          // not anything in front of the camera moves.
          stateStore.signal.map(_.signalZoom).combineWith(tick.signal) --> { _ =>
            val element = pane.ref
            val box = element.getBoundingClientRect()
            val ratio = dom.window.devicePixelRatio
            val width = math.max(1, (box.width * ratio).round.toInt)
            val height = math.max(1, (box.height * ratio).round.toInt)
            if element.width != width || element.height != height then
              element.width = width
              element.height = height
            val raw = signals.window(SignalGraph.WindowSamples)
            val corrected = CommonMode.remove(raw)
            SignalGraph.draw(
              element,
              Seq(
                // The raw channel is kept alongside for now, purely to confirm that the steps it shows are absent
                // from the corrected one. Drop this trace once that has been seen.
                SignalGraph.Trace(raw.getOrElse(quadrant, Seq.empty), stroke = "rgb(148 148 160 / 55%)", width = 1),
                SignalGraph.Trace(corrected.getOrElse(quadrant, Seq.empty), stroke = "#22c55e", width = 2)
              ),
              stateStore.current.signalZoom,
              grid = "rgb(128 128 128 / 35%)"
            )
          }
        )

      def toggle(label: Signal[String], onToggle: () => Unit): Element =
        button(cls := "logout-link", typ := "button", child.text <-- label, onClick --> (_ => onToggle()))

      div(
        cls := "acquirer-view",
        onMountCallback(_ => startCamera()),
        onUnmountCallback(_ => release()),
        div(
          cls := "screen acquirer",
          h1(cls := "screen-title", "Signal acquirer"),
          div(
            cls := "camera",
            frame,
            child <-- cameraState.signal.map:
              case CameraState.Idle                     => emptyNode
              case CameraState.Starting                 => p(cls := "camera-status", "Waiting for camera permission…")
              case CameraState.Streaming(width, height) =>
                p(
                  cls := "camera-status",
                  child.text <-- measuredHz.signal.map:
                    case Some(hz) => f"Capturing $width×$height at $hz%.1f Hz"
                    case None     => s"Capturing $width×$height…"
                )
              case CameraState.Unavailable(message) =>
                div(
                  cls := "camera-status",
                  p(cls := "error", message),
                  button(cls := "back-button", typ := "button", "Try again", onClick --> (_ => startCamera()))
                )
          ),
          div(
            cls := "rep-count",
            span(cls := "rep-count-value", child.text <-- repCount.signal.map(_.toString)),
            span(cls := "rep-count-label", "reps")
          ),
          div(
            cls := "acquirer-actions",
            toggle(
              stateStore.signal.map(state => if state.mirrored then "Unmirror" else "Mirror"),
              () => stateStore.update(current => current.copy(mirrored = !current.mirrored))
            ),
            toggle(
              stateStore.signal.map(state => if state.showSignals then "Hide signals" else "Show signals"),
              () => stateStore.update(current => current.copy(showSignals = !current.showSignals))
            ),
            toggle(
              // Names what the click will do, like the toggles beside it. What the traces are drawn at now is
              // stated on each pane, so this control has no reason to report status as well.
              stateStore.signal.map(state => s"Scale → ${SignalZoom.label(SignalZoom.next(state.signalZoom))}"),
              () => stateStore.update(current => current.copy(signalZoom = SignalZoom.next(current.signalZoom)))
            ),
            button(cls := "back-button", typ := "button", "Back", onClick --> (_ => show(Screen.Selection))),
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
        ),
        // Deliberately outside the panel: a debugging aid sitting under the whole acquirer, not part of it.
        child <-- stateStore.signal
          .map(_.showSignals)
          .map:
            case false => emptyNode
            case true  => div(cls := "signal-graphs", Quadrant.All.map(graphPane))
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
          .map(state => (state.user, state.screen))
          .map:
            // The acquirer keeps its own Back and Logout below the count, so the global bar would only duplicate it.
            case (Present(_), Screen.Acquirer) => emptyNode
            case (Present(_), _)               => userActions
            case _                             => emptyNode,
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
