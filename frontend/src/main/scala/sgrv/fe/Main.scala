package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.AboutInfo
import sgrv.api.CountingSession
import sgrv.api.CurrentUser
import sgrv.fe.acquire.{
  Camera,
  CameraDevice,
  CameraState,
  FrameSampler,
  LockState,
  RepCounter,
  RepCountStore,
  RepProgressReporter,
  RestPhase,
  CaptureState,
  TraceCapture,
  Quadrant,
  QuadrantSignals,
  Sample,
  SignalGraph
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
    val repCountStore = RepCountStore(localStorage)

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
      // Reps counted before the current detector run, which the detector itself knows nothing about: those carried
      // over from a previous page load, and those counted before a camera switch. Shown at once, so a reload mid-set
      // does not look as though it lost the count while the detector spends its first seconds finding the cadence.
      var baseline = repCountStore.restore(js.Date.now())
      val repCount = Var(baseline)
      val lock = Var[LockState](LockState.Acquiring(0, 0))
      // Derived from the camera itself rather than chosen: one on the same side as the screen is shown mirrored,
      // one facing away is not. Not persisted, since it belongs to the hardware rather than to the user.
      val mirrored = Var(false)
      val devices = Var(Seq.empty[CameraDevice])
      val currentDevice = Var(Option.empty[String])
      val menuOpen = Var(false)
      // Where each channel's peak falls within a rep, once the buffer has shown the hand at rest. Kept per quadrant
      // and retained: the opening stillness scrolls out of the minute, but what it established stays true, and a
      // leader switch should show that leader's own offset rather than the previous one's.
      val restOffsets = Var(Map.empty[Quadrant, Double])
      val capture = Var[CaptureState](CaptureState.Idle)
      val counter = RepCounter()
      val reporter = RepProgressReporter(http)
      var totalSamples = 0
      val signals = QuadrantSignals()
      val tick = Var(0)
      // Opening a camera is asynchronous, and this view can be gone before it finishes. Without this the stream
      // arrives to a dead view, starts a sampler nobody stops, and that sampler goes on writing the stored count
      // from behind whatever the user is actually looking at.
      var live = true
      // Captured when the camera opens, so a trace can say what this device's camera reported about itself.
      var cameraReport = Option.empty[String]
      var stream: Option[dom.MediaStream] = None
      var sampler: Option[FrameSampler] = None

      val video = videoTag(cls := "camera-video")
      // The overlay's lines sit at 50% of this box, so the box must be exactly the frame: its aspect ratio is set
      // from the stream once known, keeping the drawn quadrants aligned with the sampled ones.
      val frame = div(
        cls := "camera-frame",
        cls("mirrored") <-- mirrored.signal,
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
        totalSamples += 1
        val window = signals.window(signals.capacity)
        val reading = counter.update(window, totalSamples)
        reading.lock match
          case LockState.Locked(channel, _, _) =>
            RestPhase.offsetOf(window, channel).foreach(offset => restOffsets.update(_ + (channel -> offset)))
          case _ => ()
        val total = baseline + reading.count
        // Written on change rather than on every sample: ten writes a second would record nothing new between reps.
        // The sample's own timestamp dates the reading, so the age a later load measures is the age of the last rep
        // rather than of the last frame.
        if total != repCount.now() then repCountStore.save(total, sample.atMillis)
        repCount.set(total)
        lock.set(reading.lock)
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
        // Stopping the tracks is not enough on its own: while the video element still holds the stream, the browser
        // can keep the camera powered and its indicator lit after the view has gone. Letting go of it here is what
        // actually turns the camera off.
        Camera.detach(video.ref)
        firstSampleAt = None
        sampleCount = 0
        measuredHz.set(None)
        signals.clear()
        restOffsets.set(Map.empty)
        // Detection starts over from nothing, but the reps already counted stand: switching cameras mid-set is a
        // change of viewpoint, not a new workout. Absorbing them into the baseline first is what keeps them.
        baseline += counter.reading.count
        counter.reset()
        totalSamples = 0
        repCount.set(baseline)
        lock.set(LockState.Acquiring(0, 0))

      def startCamera(deviceId: Option[String] = None): Unit =
        if cameraState.now() != CameraState.Starting then
          cameraState.set(CameraState.Starting)
          Camera
            .start(deviceId)
            .onComplete:
              case Success(opened) if !live =>
                // Torn down while the camera was opening: release it rather than sample into nothing.
                Camera.stop(opened)
              case Success(opened) =>
                stream = Some(opened)
                cameraReport = Camera.report(opened)
                val element = video.ref
                prepare(element)
                element.asInstanceOf[js.Dynamic].srcObject = opened.asInstanceOf[js.Any]
                val _ = element.play()
                val (width, height) = Camera.resolution(opened).getOrElse((0, 0))
                if width > 0 && height > 0 then frame.ref.style.setProperty("aspect-ratio", s"$width / $height")
                mirrored.set(Camera.mirrors(Camera.facing(opened)))
                currentDevice.set(Camera.deviceIdOf(opened))
                // Labels stay blank until permission is granted, so the list is only worth reading now.
                Camera.videoInputs().foreach(devices.set)
                cameraState.set(CameraState.Streaming(width, height))
                val started = FrameSampler(element, onSample, () => mirrored.now())
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
          // Marks the channel the count is actually being taken from. Which quadrant wins is decided by signal
          // power, so it moves with the lighting rather than with the exercise, and watching it move is the point:
          // it explains where a tick lands in the movement, which the count alone cannot.
          div(
            cls := "leader-dot",
            cls("shown") <-- lock.signal.map:
              case LockState.Locked(channel, _, _) => channel == quadrant
              case _                               => false
            ,
            // The status line already names the leader in words; this is the same fact placed on the trace.
            aria.hidden := true
          ),
          pane,
          // Redraw on every sample, so the trace keeps scrolling on a still scene too: samples arrive whether or
          // not anything in front of the camera moves.
          tick.signal --> { _ =>
            val element = pane.ref
            val box = element.getBoundingClientRect()
            val ratio = dom.window.devicePixelRatio
            val width = math.max(1, (box.width * ratio).round.toInt)
            val height = math.max(1, (box.height * ratio).round.toInt)
            if element.width != width || element.height != height then
              element.width = width
              element.height = height
            val recent = signals.window(SignalGraph.WindowSamples)
            SignalGraph.draw(
              element,
              Seq(SignalGraph.Trace(recent.getOrElse(quadrant, Seq.empty), stroke = "#22c55e", width = 2)),
              grid = "rgb(128 128 128 / 35%)"
            )
          }
        )

      /** One entry in the menu. Choosing closes it, so the picture is not left obscured. */
      def menuItem(label: String, act: () => Unit): Element =
        button(cls := "menu-item", typ := "button", label, onClick --> (_ => { menuOpen.set(false); act() }))

      div(
        cls := "acquirer-view",
        onMountCallback { _ =>
          startCamera()
          // Reads the total rather than being pushed it, so a tick reports whatever is current at the moment it
          // fires and no report can be left describing a count that has since moved on.
          reporter.start(() => repCount.now())
        },
        onUnmountCallback { _ =>
          live = false
          reporter.stop()
          release()
        },
        div(
          cls := "screen acquirer",
          div(
            cls := "acquirer-header",
            h1(cls := "screen-title", "Signal acquirer"),
            button(
              cls := "menu-button",
              typ := "button",
              aria.label := "Menu",
              aria.expanded <-- menuOpen.signal,
              "\u2630",
              onClick --> (_ => menuOpen.update(open => !open))
            )
          ),
          // A backdrop so a tap anywhere else dismisses the menu, which is what a phone expects.
          child <-- menuOpen.signal.map:
            case false => emptyNode
            case true  => div(cls := "menu-backdrop", onClick --> (_ => menuOpen.set(false))),
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
            cls := "acquirer-footer",
            button(
              cls := "reset-button",
              typ := "button",
              // U+21BA, the anticlockwise open circle arrow: monochrome, present in the system fonts of every
              // platform this runs on, and unambiguous without a caption.
              "\u21ba",
              aria.label := "Reset the count",
              title := "Reset the count",
              onClick --> { _ =>
                baseline = 0
                counter.zeroCount()
                // Cleared rather than saved as zero: a reload should find nothing to resume, rather than a zero that
                // goes on being resumed for the rest of the retention window.
                repCountStore.clear()
                repCount.set(counter.reading.count)
              }
            ),
            // Says which of the three it is doing rather than letting a stalled count look like a steady one.
            p(
              cls := "lock-state",
              child.text <-- lock.signal
                .combineWith(restOffsets.signal)
                .map: (state, offsets) =>
                  state match
                    case LockState.Acquiring(samples, needed) =>
                      val seconds = math.max(0, needed - samples) / 10
                      if needed == 0 then "Waiting for the camera…" else s"Finding a cadence… about ${seconds}s"
                    case LockState.Searching => "No steady cadence — paused"
                    // Kept short enough to sit on one line between the reset control and its counterweight.
                    case LockState.Locked(channel, partner, periodSeconds) =>
                      // How far into a rep this channel's tick lands, when the buffer has been able to say. Shown while
                      // it is only a diagnostic: it is the number that decides whether a tick agrees with the exerciser.
                      val phase = offsets.get(channel).fold("")(offset => f" · φ$offset%.2f")
                      f"Counting $channel+$partner · $periodSeconds%.1fs/rep$phase"
            ),
            // Balances the reset control on the other side, so the reading is centred on the panel rather than on
            // whatever is left of it. Hidden from assistive technology: it carries nothing to announce.
            div(cls := "footer-spacer", aria.hidden := true)
          ),
          div(
            cls := "acquirer-actions",
            cls("open") <-- menuOpen.signal,
            // Shown only when there is somewhere to switch to.
            child <-- devices.signal
              .combineWith(currentDevice.signal)
              .map: (available, current) =>
                Camera.nextDevice(available, current) match
                  case None       => emptyNode
                  case Some(next) =>
                    val name = CameraDevice.nameOf(next, available.indexWhere(_.deviceId == next.deviceId))
                    menuItem(
                      s"Switch to $name",
                      // The scene changes entirely, so the buffered signal and the count start again: what came
                      // before belongs to a different view of the world.
                      () => { release(); startCamera(Some(next.deviceId)) }
                    ),
            // Kept in the menu rather than on the panel: capturing is for working on the detector, not for
            // working out, and a control that stops a set is worth a deliberate tap.
            child <-- capture.signal.map: state =>
              val label = state match
                case CaptureState.Idle            => "Capture signal trace"
                case CaptureState.Sending         => "Capturing…"
                case CaptureState.Captured(reps)  => s"Captured at $reps reps"
                case CaptureState.Failed(message) => s"Capture failed: $message"
              button(
                cls := "menu-item",
                typ := "button",
                label,
                disabled := state == CaptureState.Sending,
                // Deliberately does not close the menu: the outcome is reported on this very item, and closing
                // would hide the one thing the tap was for.
                onClick --> { _ =>
                  if TraceCapture.worthSending(signals) then
                    TraceCapture.send(
                      http,
                      TraceCapture.of(signals, repCount.now(), lock.now(), None, cameraReport),
                      capture.set
                    )
                  else capture.set(CaptureState.Failed("nothing recorded yet"))
                }
              )
            ,
            menuItem("About", () => openAbout()),
            menuItem("Back", () => show(Screen.Selection)),
            menuItem("Logout", () => logout())
          )
        ),
        // Below the panel and laid out as the quadrants themselves are, so a trace sits where the movement that
        // produced it appeared on screen.
        div(
          cls := "signal-graphs",
          Seq(Quadrant.Q2, Quadrant.Q1, Quadrant.Q3, Quadrant.Q4).map(graphPane)
        )
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
          .map(Shell.of)
          .distinct
          .map:
            // The acquirer keeps its own Back and Logout below the count, so the global bar would only duplicate it.
            case Shell.SignedIn(_, Screen.Acquirer) => emptyNode
            case Shell.SignedIn(_, _)               => userActions
            case _                                  => emptyNode,
        div(
          cls := "content",
          // Distinct, and on the shell rather than the whole state: without it every write to FrontendState builds a
          // new view, and on the acquirer that means a second camera, a second detector and a second writer to the
          // stored count -- one of them invisible.
          child <-- stateStore.signal
            .map(Shell.of)
            .distinct
            .map:
              case Shell.Blank => emptyNode
              case Shell.Login =>
                // Before authentication has been attempted, offer the only thing an anonymous visitor can do.
                div(cls := "home", a(cls := "login-button", href := "/auth/login", "Login with Google"))
              case Shell.AuthenticationFailed(message) =>
                div(cls := "home", p(cls := "error", s"Authentication failed: $message"))
              case Shell.SignedIn(displayName, screen) => signedInView(displayName, screen)
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
