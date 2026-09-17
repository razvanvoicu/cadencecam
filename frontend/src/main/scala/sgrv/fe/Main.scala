package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.AboutInfo
import sgrv.api.AcquirerPresence
import sgrv.api.CountingSession
import sgrv.api.CurrentUser
import sgrv.fe.acquire.{
  Adjustable,
  Camera,
  CameraAdjust,
  CameraDevice,
  CameraState,
  FrameSampler,
  LockState,
  RepCounter,
  RepCountStore,
  ResumedRun,
  RepProgressReporter,
  RestPhase,
  StatusLine,
  CaptureState,
  TraceCapture,
  Quadrant,
  QuadrantSignals,
  Sample,
  SignalGraph
}
import sgrv.api.{AccountSettings, CountsBy, ExerciseType, LiveCommand, LiveReading, LiveState, PeerRole, WeightUnit}
import sgrv.fe.bench.BenchView
import sgrv.fe.live.PeerLink
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

    // Asked once, at the start: Chrome on Android keeps the handset's model out of its user agent, and the only way
    // to learn it is an explicit request that answers later.
    Device.learn()

    def show(screen: Screen): Unit = stateStore.update(_.copy(screen = screen))

    /** Raised while this device waits to be told whether to displace another one that is already counting.
      *
      * One acquirer per account is enforced by the relay, which stands the older device down. That is the right outcome
      * and the wrong way to arrive at it unannounced: on a bench with four handsets signed into one account, a phone
      * that silently stopped counting looked like a phone that had crashed. So it is asked for first.
      */
    val takeoverPending = Var(false)

    /** Whether this account already has a device counting, as far as the last answer from the server goes.
      *
      * Decides what a fresh login is shown. With nothing counting there is nothing to choose between -- the account
      * needs a counter before a dashboard has anything to watch -- so the device goes straight to counting. With a
      * counter already running, the choice is real and worth making deliberately.
      */
    val accountCounting = Var(false)

    /** What this account has set, as last read from its record.
      *
      * Held here rather than in the dashboard, because more than one screen asks and because a screen that fetched its
      * own copy on every mount would show the defaults for as long as the request took -- which on the dashboard means
      * a calorie figure that is wrong first and right a moment later.
      *
      * Starts at the defaults rather than at nothing: every figure that depends on these has a sensible value from the
      * first paint, and the account's own numbers replace them when they arrive.
      */
    val accountSettings = Var(AccountSettings.Initial)

    /** Whether the settings panel is up. Deliberately not in [[FrontendState]]: it is a panel over whatever screen is
      * running, and persisting it would reopen it on a reload -- over a camera that then keeps counting behind it.
      */
    val settingsOpen = Var(false)

    /** Reads the account's settings. Called when a session is confirmed, which is what "on every login" means here.
      *
      * A failure leaves the defaults in place and says so in the console only. There is nothing a person can do about
      * it from the dashboard, and refusing to draw the screen over it would turn a wrong factor into no screen at all.
      */
    def loadSettings(): Unit =
      http
        .get(AccountSettings.Path)
        .flatMap(response => response.text())
        .map(_.fromJson[AccountSettings])
        .onComplete:
          case Success(Right(settings)) => accountSettings.set(settings)
          case Success(Left(details))   => dom.console.warn(s"Ignoring unreadable account settings: $details")
          case Failure(error) => dom.console.warn(s"Could not read the account settings: ${errorMessage(error)}")

    /** Becomes the acquirer, asking first if the account already has one.
      *
      * The check can fail -- an offline device, a signed-out session -- and a failure here must not stand between
      * someone and their camera, so it proceeds. The relay still guarantees there is only one.
      */
    /** Where a fresh login lands: counting if the account has no counter, the choice of roles if it has.
      *
      * The check can fail -- an offline device, a session that has just expired -- and a failure must not stand between
      * someone and their camera, so it counts. Taking the role is what settles it either way.
      */
    def startByPresence(): Unit =
      http
        .get(AcquirerPresence.Path)
        .flatMap(response => response.text())
        .map(_.fromJson[AcquirerPresence])
        .onComplete:
          case Success(Right(AcquirerPresence(true))) => accountCounting.set(true)
          case _                                      =>
            accountCounting.set(false)
            show(Screen.Acquirer)

    def acquireRole(): Unit =
      http
        .get(AcquirerPresence.Path)
        .flatMap(response => response.text())
        .map(_.fromJson[AcquirerPresence])
        .onComplete:
          case Success(Right(AcquirerPresence(true))) => takeoverPending.set(true)
          case _                                      => show(Screen.Acquirer)

    /** Claims the counter's role, which is also what opens the account's session when it has none.
      *
      * Sent as the camera view appears rather than when the role is chosen, because the view is arrived at by more than
      * one route -- a fresh login going straight to counting, a deliberate takeover, a device returning to the role it
      * already held -- and every one of them means this device is the counter from here.
      *
      * A failure is left as a warning. Nothing downstream can be faked without a session, and the device finds out soon
      * enough: its first progress report is refused and it stands down, which is the truthful outcome.
      */
    def takeCounterRole(): Unit =
      val init = new dom.RequestInit:
        method = dom.HttpMethod.POST
      http
        .send(AcquirerPresence.Path, init)
        .onComplete:
          case Success(response) if response.ok => ()
          case Success(response)                =>
            dom.console.warn(s"Could not take the counting role: HTTP ${response.status}")
          case Failure(error) =>
            dom.console.warn(s"Could not take the counting role: ${errorMessage(error)}")

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
        loadSettings()
        // Only for a login that has just landed on the picker: a device returning to the role it already held keeps
        // it, and must not be sent to the camera because some other device happens to be free.
        if !stateStore.restoredUserEmail.contains(email) then startByPresence()
      case Success(MeResult(session, _)) => updateUser(session)
      case Failure(error)                => updateUser(AuthenticationFailed(errorMessage(error)))

    def openAbout(): Unit =
      val signedIn = stateStore.current.user match
        case SignedIn(_, _) | Restoring(_, _) => true
        case _                                => false
      // Nobody to ask before signing in: the build endpoint needs a session, so asking only ever produced a 401
      // dressed up as a failure, and tripped the session handling on the way out. What is worth knowing there --
      // which bundle this browser is running -- is known locally.
      if !signedIn then stateStore.update(_.copy(aboutState = AboutState.LocalOnly))
      else
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

    /** The login screen's menu: the same sheet the app's other screens open, minus everything that needs an account.
      *
      * A menu rather than the bare About link it replaces, because the two documents belong in front of someone who has
      * not signed in yet -- that is the moment they are deciding whether to -- and a row of links across the top of a
      * phone is not where anybody would look for them.
      */
    def guestMenu: Element =
      val open = Var(false)
      div(
        cls := "user-actions",
        Menu.toggle(open),
        Menu.backdrop(open),
        Menu.sheet(open, Menu.item(open, "About", () => openAbout()), Menu.documentItems(open))
      )

    def roleChoice(modifier: String, screen: Screen, title: String, description: String): Element =
      button(
        cls := s"mode-button $modifier",
        typ := "button",
        onClick --> (_ => if screen == Screen.Acquirer then acquireRole() else show(screen)),
        span(cls := "mode-title", title),
        span(cls := "mode-description", description)
      )

    def selection(displayName: String): Element =
      div(
        cls := "selection",
        h1(cls := "welcome", s"Hello, $displayName!"),
        p(
          cls := "selection-prompt",
          child.text <-- accountCounting.signal.map: counting =>
            if counting then "Another device is counting for this account. Choose what this one does."
            else "Choose what this device does in the next session."
        ),
        div(
          cls := "mode-choices",
          child <-- accountCounting.signal.map: counting =>
            if counting then
              roleChoice(
                "mode-acquirer",
                Screen.Acquirer,
                "Take over counting",
                "Count on this device instead. The device counting now returns to its home screen and releases its camera."
              )
            else
              roleChoice(
                "mode-acquirer",
                Screen.Acquirer,
                "Counter",
                "Aim this device's camera at the movement and let it count the reps."
              )
          ,
          roleChoice(
            "mode-dashboard",
            Screen.Dashboard,
            "Dashboard",
            "Watch the live rep count arriving from the counting device."
          ),
          roleChoice(
            "mode-bench",
            Screen.Bench,
            "Test",
            "Show a movement of known cadence and measure the count against it. For a desktop screen."
          )
        )
      )

    def signedInView(displayName: String, screen: Screen): Element =
      screen match
        case Screen.Selection => selection(displayName)
        case Screen.Acquirer  => acquirer()
        case Screen.Dashboard => dashboard()
        case Screen.Bench     => BenchView(http, () => show(Screen.Selection))

    /** Watches the count arriving from whichever device is acquiring for this account.
      *
      * It holds no count of its own. Everything shown here is a copy of what the acquirer last said, and the two
      * controls ask that device to do the things its own controls do rather than acting locally — so the acquirer stays
      * the single authority on the tally, and a dashboard that has been away simply catches up.
      *
      * Laid out as a column of rows sized by their content, with the panel taking what is left. Nothing is positioned
      * absolutely and no row has a fixed height, so a landscape arrangement later is a change of direction on the
      * container rather than a rewrite.
      */
    /** The settings panel: a weight, a default factor, and the exercises this account counts.
      *
      * Edited against a copy and written back only on Save, so leaving by Cancel leaves nothing behind. The copy is
      * taken when the panel opens rather than held permanently, which is what makes Cancel a real undo of everything
      * done inside it.
      */
    def settingsPanel(): Element =
      val draft = Var(accountSettings.now())
      val saving = Var(false)
      val failed = Var(Option.empty[String])

      def close(): Unit =
        settingsOpen.set(false)

      def commit(): Unit =
        if !saving.now() then
          saving.set(true)
          failed.set(None)
          val next = draft.now()
          val init = new dom.RequestInit:
            method = dom.HttpMethod.PUT
            headers = js.Dictionary("Content-Type" -> "application/json")
            body = next.toJson
          http
            .send(AccountSettings.Path, init)
            .onComplete: outcome =>
              saving.set(false)
              outcome match
                case Success(response) if response.ok =>
                  // From the draft rather than from the response body, which says the same thing: the screens behind
                  // this one should show the new figures the moment it closes, not one round trip later.
                  accountSettings.set(next)
                  close()
                case Success(response) =>
                  failed.set(Some(s"The server would not save these settings (HTTP ${response.status})."))
                case Failure(error) =>
                  failed.set(Some(errorMessage(error)))

      /** A number the user types, taken only when it parses and is above zero.
        *
        * Rejecting rather than clamping: a half-typed "0.", or a field someone has just emptied to retype, is not a
        * statement that their weight is nothing, and writing one into the draft would make the field fight the typing.
        */
      def numberField(shown: String, onValue: Double => Unit, extra: Modifier[HtmlElement]*): Element =
        input(
          cls := "settings-number",
          typ := "number",
          stepAttr := "any",
          minAttr := "0",
          defaultValue := shown,
          onInput.mapToValue --> { entered =>
            entered.trim.toDoubleOption.filter(parsed => parsed > 0 && parsed.isFinite).foreach(onValue)
          },
          extra
        )

      /** A factor, typed or nudged.
        *
        * Typed, because a factor calibrated against a real machine is a number like 0.0125 and no amount of tapping
        * gets there from one. Nudged proportionally, because the same control also has to serve a factor of two.
        */
      def factorField(value: Signal[Double], onChange: Double => Unit, current: () => Double): Element =
        div(
          cls := "settings-stepper",
          button(
            cls := "stepper-button",
            typ := "button",
            aria.label := "Less",
            "–",
            onClick --> (_ => onChange(Effort.nudged(current(), up = false)))
          ),
          // Redrawn on the value the buttons put there, so a nudge is reflected in the field while typing into it is
          // not fought by a redraw of what is being typed.
          child <-- value
            .map(Effort.factorText)
            .distinct
            .map: shown =>
              numberField(shown, onChange, cls := "settings-factor"),
          button(
            cls := "stepper-button",
            typ := "button",
            aria.label := "More",
            "+",
            onClick --> (_ => onChange(Effort.nudged(current(), up = true)))
          )
        )

      def exerciseRow(id: String, initial: ExerciseType, updates: Signal[ExerciseType]): Element =
        def edit(change: ExerciseType => ExerciseType): Unit =
          draft.update(current =>
            current.copy(exercises = current.exercises.map(one => if one.id == id then change(one) else one))
          )
        def now(): ExerciseType = draft.now().exercises.find(_.id == id).getOrElse(initial)

        div(
          cls := "exercise-row",
          cls("chosen") <-- draft.signal.map(_.selected.contains(id)),
          // The whole row selects, because "which exercise is this dashboard reporting" is the question the screen is
          // mostly opened to answer, and a radio button hidden among the fields answers it too quietly.
          onClick --> (_ => draft.update(current => current.copy(selected = Some(id)))),
          div(
            cls := "exercise-identity",
            input(
              cls := "settings-text",
              typ := "text",
              placeholder := "Exercise",
              defaultValue := initial.name,
              onInput.mapToValue --> (entered => edit(_.copy(name = entered)))
            ),
            input(
              cls := "settings-text settings-equipment",
              typ := "text",
              placeholder := "Equipment, setting, weight…",
              defaultValue := initial.equipment.getOrElse(""),
              onInput.mapToValue --> { entered =>
                edit(_.copy(equipment = Option(entered.trim).filter(_.nonEmpty)))
              }
            )
          ),
          div(
            cls := "exercise-field",
            span(cls := "field-label", "Counts by"),
            div(
              cls := "settings-toggle",
              button(
                cls := "toggle-option",
                typ := "button",
                cls("on") <-- updates.map(_.countsBy == CountsBy.RepCount),
                "Rep count",
                onClick --> (_ => edit(_.copy(countsBy = CountsBy.RepCount)))
              ),
              button(
                cls := "toggle-option",
                typ := "button",
                cls("on") <-- updates.map(_.countsBy == CountsBy.Frequency),
                "Frequency",
                onClick --> (_ => edit(_.copy(countsBy = CountsBy.Frequency)))
              )
            )
          ),
          div(
            cls := "exercise-field",
            span(cls := "field-label", "Factor"),
            factorField(updates.map(_.factor), factor => edit(_.copy(factor = factor)), () => now().factor)
          ),
          button(
            cls := "exercise-remove",
            typ := "button",
            aria.label := "Remove this exercise",
            title := "Remove this exercise",
            "×",
            onClick --> { event =>
              event.stopPropagation()
              draft.update: current =>
                current.copy(
                  exercises = current.exercises.filterNot(_.id == id),
                  selected = current.selected.filterNot(_ == id)
                )
            }
          )
        )

      def addExercise(): Unit =
        draft.update: current =>
          val id = f"e${js.Date.now().toLong}%d-${scala.util.Random.nextInt(0x1000)}%03x"
          current.copy(
            exercises = current.exercises :+ ExerciseType(id, "", None, CountsBy.RepCount, current.defaultFactor),
            // A new exercise is almost always the one about to be done, and selecting it saves the second tap.
            selected = Some(id)
          )

      div(
        cls := "settings-overlay",
        div(
          cls := "settings-screen",
          role := "dialog",
          div(
            cls := "settings-header",
            h2("Settings"),
            span(cls := "settings-note", "saved to your account"),
            button(cls := "about-close", typ := "button", title := "Close", onClick --> (_ => close()), "×")
          ),
          div(
            cls := "settings-body",
            div(
              cls := "settings-card",
              div(
                cls := "settings-field",
                span(cls := "field-label", "Your weight"),
                div(
                  cls := "settings-weight",
                  child <-- draft.signal
                    .map(_.weightUnit)
                    .distinct
                    .map: unit =>
                      numberField(
                        f"${WeightUnit.show(draft.now().weightKilograms, unit)}%.1f",
                        entered => draft.update(_.copy(weightKilograms = WeightUnit.toKilograms(entered, unit)))
                      ),
                  div(
                    cls := "settings-toggle",
                    WeightUnit.values.toSeq.map: unit =>
                      button(
                        cls := "toggle-option",
                        typ := "button",
                        cls("on") <-- draft.signal.map(_.weightUnit == unit),
                        WeightUnit.label(unit),
                        // The stored weight does not move: only what it is shown in does.
                        onClick --> (_ => draft.update(_.copy(weightUnit = unit)))
                      )
                  )
                ),
                span(cls := "field-hint", "Kept once, used by every exercise.")
              ),
              div(
                cls := "settings-field",
                span(cls := "field-label", "Default factor"),
                factorField(
                  draft.signal.map(_.defaultFactor),
                  factor => draft.update(_.copy(defaultFactor = factor)),
                  () => draft.now().defaultFactor
                ),
                span(cls := "field-hint", "Used by a new exercise until you set its own.")
              )
            ),
            div(
              cls := "settings-card settings-exercises",
              div(
                cls := "settings-card-header",
                span(cls := "field-label", "Exercises"),
                button(cls := "add-exercise", typ := "button", "+ Add exercise", onClick --> (_ => addExercise()))
              ),
              div(
                cls := "exercise-list",
                children <-- draft.signal.map(_.exercises).split(_.id)(exerciseRow)
              ),
              child <-- draft.signal
                .map(_.exercises.isEmpty)
                .distinct
                .map:
                  case false => emptyNode
                  case true  =>
                    p(
                      cls := "field-hint",
                      "With no exercise chosen the dashboard counts reps at the default factor."
                    )
            ),
            p(
              cls := "settings-explainer",
              "The chosen exercise drives the dashboard. Rep count multiplies reps by weight and factor; frequency " +
                "sums each rep's rate instead, so faster reps count for more."
            )
          ),
          child <-- failed.signal.map:
            case None          => emptyNode
            case Some(message) => p(cls := "error settings-error", message),
          div(
            cls := "settings-actions",
            button(cls := "back-button", typ := "button", "Cancel", onClick --> (_ => close())),
            button(
              cls := "mode-button",
              typ := "button",
              disabled <-- saving.signal,
              child.text <-- saving.signal.map(if _ then "Saving…" else "Save"),
              onClick --> (_ => commit())
            )
          )
        )
      )

    def dashboard(): Element =
      val menuOpen = Var(false)
      val live = Var(Option.empty[LiveState])
      val connected = Var(false)

      val reading = live.signal.map(_.flatMap(_.reading))
      val reps = reading.map(_.fold(0)(_.reps))
      val elapsedSeconds = reading.map(_.fold(0.0)(_.elapsedSeconds))

      /** The last few reps, as they were when each was counted. Kept here rather than sent, because a pace is a
        * property of the reps a watcher has seen and the counter has no reason to hold a second window of its own.
        */
      val window = Var(Vector.empty[Effort.RepMark])

      /** Everything on the screen is a function of the reading, the window and the account's settings, so the three are
        * combined once and every figure below is read off the result. Separately derived signals would each re-evaluate
        * the same arithmetic and, worse, could disagree about which reading they were describing.
        */
      val figures = reading
        .combineWith(window.signal, accountSettings.signal)
        .map: (latest, marks, settings) =>
          val counted = latest.fold(0)(_.reps)
          val elapsedMinutes = latest.fold(0.0)(_.elapsedSeconds) / 60.0
          val calories = Effort.calories(counted, latest.fold(0.0)(_.cadenceSum), settings)
          (counted, elapsedMinutes, calories, Effort.pace(marks), Effort.burn(marks, settings))

      val boundary = elapsedSeconds.map(seconds => Effort.boundaryLabel(Effort.boundaryMinutes(seconds / 60.0)))

      def figure(
          of: ((Int, Double, Double, Option[Double], Option[Double])) => Option[Double],
          show: Double => String
      ): Signal[String] =
        figures.map(values => of(values).fold(Effort.Absent)(show))

      val repsPerMinute = figure((_, _, _, pace, _) => pace, Effort.rate)
      val repsAtBoundary =
        figure(
          (reps, minutes, _, pace, _) => pace.flatMap(Effort.atBoundary(reps.toDouble, _, minutes)),
          Effort.grouped
        )
      val calories = figures.map((_, _, calories, _, _) => Effort.grouped(calories))
      val caloriesPerMinute = figure((_, _, _, _, burn) => burn, Effort.rate)
      val caloriesAtBoundary =
        figure((_, minutes, calories, _, burn) => burn.flatMap(Effort.atBoundary(calories, _, minutes)), Effort.grouped)

      /** Three situations that a single "waiting" would render identical, told apart.
        *
        * A dashboard whose own link is down, one connected to an account with nothing counting, and one connected to a
        * device that has not yet found a cadence are different problems with different remedies, and only the last of
        * them is the app working normally.
        */
      val status = live.signal
        .combineWith(connected.signal)
        .map:
          case (_, false)                               => "Connecting…"
          case (Some(LiveState(true, Some(latest))), _) => latest.status
          case (Some(LiveState(true, None)), _)         => "Waiting for the first rep"
          case (Some(LiveState(false, _)), _)           => "No device is counting yet"
          case (None, _)                                => StatusLine.Waiting

      def readUpdate(text: String): Unit =
        text.fromJson[LiveState] match
          case Right(state) =>
            live.set(Some(state))
            state.reading.foreach(mark)
          case Left(details) => dom.console.warn(s"Ignoring an unreadable update: $details")

      def mark(latest: LiveReading): Unit =
        window.update: marks =>
          Effort.noting(
            marks,
            Effort.RepMark(latest.reps, latest.lastRepSeconds, latest.cadenceSum)
          )

      /** The direct link to the counting device, which the readings travel over.
        *
        * The watcher offers, because it is the one that wants the data. There is no other path: the server introduces
        * the two and then has nothing further to do with them.
        */
      val peer: PeerLink = PeerLink(
        http,
        PeerRole.Watcher,
        readUpdate,
        onOpen = () => connected.set(true),
        onClosed = () => connected.set(false)
      )

      def ask(instruction: LiveCommand): Unit =
        val _ = peer.send(instruction.toJson)

      def menuItem(label: String, act: () => Unit): Element = Menu.item(menuOpen, label, act)

      /** One of the two cards: a headline figure, and the two smaller ones that qualify it.
        *
        * The pair below is always the rate and the projection, in that order, on both cards -- so the eye learns the
        * shape once and reads the second card without having to.
        */
      def card(
          kind: String,
          label: String,
          value: Signal[String],
          rate: Signal[String],
          ahead: Signal[String]
      ): Element =
        div(
          cls := s"figure-card $kind",
          span(cls := "figure-label", label),
          span(cls := "figure-value", child.text <-- value),
          div(
            cls := "figure-footnotes",
            div(
              cls := "figure-footnote",
              span(cls := "footnote-label", "Per minute"),
              span(cls := "footnote-value", child.text <-- rate)
            ),
            div(
              cls := "figure-footnote",
              span(cls := "footnote-label", child.text <-- boundary.map(at => s"At $at")),
              span(cls := "footnote-value", child.text <-- ahead)
            )
          )
        )

      div(
        cls := "dashboard-view",
        onMountCallback { _ =>
          peer.connect()
          // Read again on arrival as well as at login: a tablet left on this screen overnight is signed in from a
          // session it has not re-established, and the settings may have been changed from the phone in the meantime.
          loadSettings()
        },
        onUnmountCallback(_ => peer.close()),
        div(
          cls := "screen dashboard",
          div(
            cls := "dashboard-header",
            div(
              cls := "dashboard-title",
              h1(
                cls := "screen-title",
                child.text <-- accountSettings.signal.map(_.selectedExercise.fold("Counting")(_.name))
              ),
              span(
                cls := "dashboard-basis",
                child.text <-- accountSettings.signal.map: settings =>
                  val how = settings.countsBy match
                    case CountsBy.RepCount  => "rep count"
                    case CountsBy.Frequency => "frequency"
                  s"factor ${Effort.factorText(settings.factor)} · $how"
              )
            ),
            div(
              cls := "dashboard-clock",
              span(cls := "clock-value", child.text <-- elapsedSeconds.map(Effort.elapsedClock)),
              Menu.toggle(menuOpen)
            )
          ),
          Menu.backdrop(menuOpen),
          div(
            cls := "figure-cards",
            card("reps-card", "Reps", reps.map(_.toString), repsPerMinute, repsAtBoundary),
            card("calories-card", "Calories", calories, caloriesPerMinute, caloriesAtBoundary)
          ),
          div(
            cls := "dashboard-status",
            button(
              cls := "reset-button",
              typ := "button",
              "↺",
              aria.label := "Reset the count",
              title := "Reset the count",
              onClick --> (_ => ask(LiveCommand.Reset))
            ),
            p(cls := "lock-state", child.text <-- status)
          ),
          Menu.sheet(
            menuOpen,
            menuItem("Settings", () => settingsOpen.set(true)),
            menuItem("Capture signal trace", () => ask(LiveCommand.CaptureTrace())),
            menuItem("About", () => openAbout()),
            Menu.documentItems(menuOpen),
            menuItem("Back", () => show(Screen.Selection)),
            menuItem("Logout", () => logout())
          )
        )
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
      val repCount = Var(baseline.count)
      val lock = Var[LockState](LockState.Acquiring(0, 0))
      // How far the movement stands above the background, kept whether or not a cadence has been found: a movement
      // too faint to count looks from the outside exactly like no movement at all.
      val signalMargin = Var(Option.empty[Double])
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
      // Composed once and used twice: shown here, and sent verbatim to whatever is watching. Re-deriving the wording
      // at the other end would let the two screens drift apart on the first change to either.
      val statusText = lock.signal
        .combineWith(restOffsets.signal)
        .map: (state, offsets) =>
          state match
            case LockState.Locked(channel, _, _) =>
              // How far into a rep this channel's tick lands, when the buffer has been able to say. Shown while it is
              // only a diagnostic: it is the number that decides whether a tick agrees with the exerciser.
              val phase = offsets.get(channel).fold("")(offset => f" · φ$offset%.2f")
              StatusLine.of(state) + phase
            case other => StatusLine.of(other)
      var latestStatus = StatusLine.of(LockState.Acquiring(0, 0))
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
      var controlNote = Option.empty[String]
      var controlsAtSample = Option.empty[Int]
      var stream: Option[dom.MediaStream] = None
      var sampler: Option[FrameSampler] = None
      // What this camera lets a person move by hand. Empty until a camera is open, and on any camera that reports its
      // settings as modes rather than ranges -- Safari offers no exposure at all, so there the panel simply is not
      // there rather than being there and inert.
      val adjustables = Var(Seq.empty[Adjustable])
      val adjustOpen = Var(false)

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

      var lastPublished = Option.empty[LiveReading]

      /** Sends a reading only when it says something new.
        *
        * Samples arrive ten times a second and almost all of them repeat the last: the count is unchanged between reps,
        * and the pace is only worth reporting to the nearest whole number a screen will show.
        */
      def publish(): Unit =
        val reading =
          LiveReading(
            repCount.now(),
            math.round(counter.repsPerMinute).toDouble,
            latestStatus,
            Device.describe(),
            counting = lock.now().isInstanceOf[LockState.Locked],
            // Both carry whatever a previous page load left behind, for the same reason the count does: a watching
            // device must not see a set restart because the phone showing it reloaded.
            // Whole seconds. The clock runs on while a set rests, so an unrounded figure would differ on every one
            // of the ten samples a second and publish ten updates a second to say so.
            elapsedSeconds = math.round(baseline.elapsedSeconds + counter.elapsedSeconds).toDouble,
            cadenceSum = baseline.cadenceSum + counter.reading.cadenceSum,
            // To a tenth, which is the sample clock's own resolution: this is what a cadence is measured over, and it
            // changes only when a rep is counted, so it costs nothing to send exactly.
            lastRepSeconds = math.round((baseline.elapsedSeconds + counter.lastRepSeconds) * 10.0) / 10.0
          )
        if !lastPublished.contains(reading) then
          // To whoever is watching, over their own link. Nothing is published when nobody is: a count with no
          // audience is the ordinary case, and there is no server left to tell.
          if peer.send(LiveState(acquiring = true, reading = Some(reading)).toJson) then lastPublished = Some(reading)

      def onSample(sample: Sample): Unit =
        signals.record(sample)
        totalSamples += 1
        // The detector's own window, not the whole buffer. The buffer is now six minutes so a trace can carry a
        // whole session, and re-filtering all of it on every sample would be five times the work at ten hertz.
        val window = signals.window(QuadrantSignals.DetectionWindow)
        val reading = counter.update(window, totalSamples)
        reading.lock match
          case LockState.Locked(channel, _, _) =>
            RestPhase.offsetOf(window, channel).foreach(offset => restOffsets.update(_ + (channel -> offset)))
          case _ => ()
        val total = baseline.count + reading.count
        // Written on change rather than on every sample: ten writes a second would record nothing new between reps.
        // The sample's own timestamp dates the reading, so the age a later load measures is the age of the last rep
        // rather than of the last frame.
        if total != repCount.now() then
          repCountStore.save(
            total,
            sample.atMillis,
            baseline.cadenceSum + reading.cadenceSum,
            baseline.elapsedSeconds + reading.elapsedSeconds
          )
        repCount.set(total)
        lock.set(reading.lock)
        signalMargin.set(reading.margin)
        publish()
        tick.update(_ + 1)
        sampleCount += 1
        firstSampleAt match
          case None        => firstSampleAt = Some(sample.atMillis)
          case Some(start) =>
            val elapsed = sample.atMillis - start
            if elapsed > 0 then measuredHz.set(Some((sampleCount - 1) * 1000.0 / elapsed))

      /** Zeroes the tally without disturbing the detection behind it. Reached from this device's own control and from a
        * watching one, which must mean the same thing on both.
        */
      /** Starts counting over from nothing: the tally, the detector, and the signal behind them.
        *
        * A full wipe rather than a zeroed tally. The button exists to discard what accrued while the user was getting
        * into position, and leaving the buffer would keep that movement working against them twice over -- its peaks
        * can still be counted, and its strength still sets the threshold that the real exercise has to clear. Vigorous
        * setting-up was measured suppressing the exercise that followed for as long as the detector's window holds it.
        *
        * The cost is the fifteen seconds the detector needs before it can lock again, and it is not a loss: the reps
        * performed in the meantime are in the buffer, and the first lock counts the run it finds there.
        */
      def resetCount(): Unit =
        baseline = ResumedRun.Nothing
        signals.clear()
        counter.reset()
        totalSamples = 0
        restOffsets.set(Map.empty)
        lock.set(LockState.Acquiring(0, 0))
        signalMargin.set(None)
        // Cleared rather than saved as zero: a reload should find nothing to resume, rather than a zero that goes on
        // being resumed for the rest of the retention window.
        repCountStore.clear()
        repCount.set(0)
        // Announced rather than left to the next change of reading. A reset is the one moment a watching device
        // needs to hear about even if nothing else has moved: it is how the bench knows its command arrived, and a
        // command silently lost left one phone counting a whole test onto the previous test's total.
        lastPublished = None
        publish()

      def captureTrace(note: Option[String] = None): Unit =
        if TraceCapture.worthSending(signals) then
          TraceCapture.send(
            http,
            TraceCapture
              .of(
                signals,
                repCount.now(),
                lock.now(),
                note,
                cameraReport,
                controlNote,
                controlsAtSample,
                Device.describe()
              ),
            capture.set
          )
        else capture.set(CaptureState.Failed("nothing recorded yet"))

      /** Carries readings out to whatever is watching, and the two commands back.
        *
        * The acquirer is the authority throughout: a command is a request to do the same thing this device's own
        * controls do, not a way to set the count from outside.
        */
      def obey(text: String): Unit =
        text.fromJson[LiveCommand] match
          case Right(LiveCommand.Reset)              => resetCount()
          case Right(LiveCommand.CaptureTrace(note)) => captureTrace(note)
          case Right(LiveCommand.Displaced)          =>
            // Another device has taken the role. Leaving this screen is what releases the camera; staying would
            // leave a phone counting into a room it no longer owns, with its own tally still climbing on screen.
            dom.console.info("Another device is now counting for this account; standing down")
            show(Screen.Selection)
          case Left(details) => dom.console.warn(s"Ignoring an unreadable command: $details")

      /** The counting end of the direct link. It answers offers rather than making them: a counter with nobody watching
        * has nothing to offer, and a watcher appearing is what starts the exchange.
        */
      lazy val peer: PeerLink = PeerLink(
        http,
        PeerRole.Counter,
        obey,
        onOpen = () => dom.console.info("A watching device is reading the count directly"),
        onClosed = () => dom.console.info("The direct link closed; readings go back over the relay")
      )

      def release(): Unit =
        sampler.foreach(_.stop())
        sampler = None
        stream.foreach(Camera.stop)
        stream = None
        adjustables.set(Seq.empty)
        // Stopping the tracks is not enough on its own: while the video element still holds the stream, the browser
        // can keep the camera powered and its indicator lit after the view has gone. Letting go of it here is what
        // actually turns the camera off.
        Camera.detach(video.ref)
        firstSampleAt = None
        sampleCount = 0
        measuredHz.set(None)
        signals.clear()
        restOffsets.set(Map.empty)
        // Detection starts over from nothing, but what was already counted stands: switching cameras mid-set is a
        // change of viewpoint, not a new workout. Absorbing it into the baseline first is what keeps it -- the whole
        // of it, since a set whose calories reset at a camera change would be no better off than one whose reps did.
        val carried = counter.reading
        baseline = ResumedRun(
          baseline.count + carried.count,
          baseline.cadenceSum + carried.cadenceSum,
          baseline.elapsedSeconds + carried.elapsedSeconds
        )
        counter.reset()
        totalSamples = 0
        repCount.set(baseline.count)
        lock.set(LockState.Acquiring(0, 0))

      def startCamera(deviceId: Option[String] = None): Unit =
        if cameraState.now() != CameraState.Starting then
          cameraState.set(CameraState.Starting)
          Camera
            .start(
              deviceId,
              outcome =>
                controlNote = Some(TraceCapture.describe(outcome))
                // The sample count at the moment they settled: a trace can then be read for whether the picture
                // changed here or somewhere else entirely.
                controlsAtSample = Some(totalSamples)
            )
            .onComplete:
              case Success(opened) if !live =>
                // Torn down while the camera was opening: release it rather than sample into nothing.
                Camera.stop(opened)
              case Success(opened) =>
                stream = Some(opened)
                cameraReport = Camera.report(opened)
                // Read after the controls have been held, so the sliders start from what the camera actually settled
                // on rather than from what it was doing while metering was still moving.
                adjustables.set(
                  (for
                    capabilities <- CameraAdjust.capabilitiesOf(opened)
                    settings <- CameraAdjust.settingsOf(opened)
                  yield CameraAdjust.adjustable(capabilities, settings)).getOrElse(Seq.empty)
                )
                val element = video.ref
                prepare(element)
                element.asInstanceOf[js.Dynamic].srcObject = opened.asInstanceOf[js.Any]
                val _ = element.play()
                // Taken from the element rather than from the track, and re-taken whenever it changes. The element's
                // intrinsic size is what is actually being painted; a track's reported settings can describe the
                // frame before the device rotated it, and can still name the old mode for a moment after a constraint
                // has been applied. Either disagreement would size the box to a shape the picture does not have, and
                // put the drawn quadrant lines somewhere the detector is not sampling.
                //
                // Assigned rather than added, so restarting the camera replaces these handlers instead of stacking
                // another copy on the same element.
                val media = element.asInstanceOf[js.Dynamic]
                val noteShape: js.Function1[dom.Event, Unit] = _ =>
                  val shown = element.videoWidth
                  val tall = element.videoHeight
                  if shown > 0 && tall > 0 then
                    frame.ref.style.setProperty("--frame-aspect", (shown.toDouble / tall).toString)
                    cameraState.set(CameraState.Streaming(shown, tall))
                media.onloadedmetadata = noteShape
                media.onresize = noteShape
                val (width, height) = Camera.resolution(opened).getOrElse((0, 0))
                if width > 0 && height > 0 then
                  frame.ref.style.setProperty("--frame-aspect", (width.toDouble / height).toString)
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
          // Unlabelled. The panes are laid out as the quadrants are, so where a trace sits already says which part of
          // the frame it came from -- and "Q2" says that to nobody who has not read the detector.
          // Marks the channel the count is taken from, and the channel corroborating it. One marker in one place,
          // coloured by role: a quadrant is never both at once, and two markers at different offsets made the same
          // fact appear in two different spots depending on which role it happened to be.
          div(
            cls := "channel-dot",
            cls("leader") <-- lock.signal.map:
              case LockState.Locked(channel, _, _) => channel == quadrant
              case _                               => false
            ,
            cls("partner") <-- lock.signal.map:
              case LockState.Locked(_, partner, _) => partner == quadrant
              case _                               => false
            ,
            // Decoration over the trace it belongs to, and nothing a screen reader can do anything with.
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

      def menuItem(label: String, act: () => Unit): Element = Menu.item(menuOpen, label, act)

      div(
        cls := "acquirer-view",
        statusText --> { text =>
          latestStatus = text
          publish()
        },
        onMountCallback { _ =>
          // Waits to be found: a watching device offers, this end answers. Until one appears there is nothing to
          // pair with, which is why the counter only listens.
          peer.connect()
          // Before anything is reported: a report carries a count into the account's session, and until the role is
          // taken there is no session to carry it into.
          takeCounterRole()
          startCamera()
          // Reads the total rather than being pushed it, so a tick reports whatever is current at the moment it
          // fires and no report can be left describing a count that has since moved on.
          // Leaving this screen is what releases the camera; staying would leave a phone counting into a room it no
          // longer owns, with its own tally still climbing on screen.
          reporter.onDisplaced = () => show(Screen.Selection)
          reporter.start(() => repCount.now())
        },
        onUnmountCallback { _ =>
          live = false
          peer.close()
          reporter.stop()
          release()
        },
        div(
          cls := "screen acquirer",
          div(
            cls := "acquirer-header",
            h1(cls := "screen-title", "Counter"),
            Menu.toggle(menuOpen)
          ),
          Menu.backdrop(menuOpen),
          div(
            cls := "camera",
            frame,
            child <-- cameraState.signal.map:
              case CameraState.Idle     => emptyNode
              case CameraState.Starting => p(cls := "camera-status", "Waiting for camera permission…")
              // One bullet per device reading this counter directly, where the resolution used to be: a counter may
              // be read by several at once, and each is its own connection rather than a shared one.
              case CameraState.Streaming(_, _) =>
                div(
                  cls := "watcher-dots",
                  children <-- peer.watchers.signal.map: many =>
                    Seq.fill(many)(span(cls := "watcher-dot", aria.hidden := true))
                )
              case CameraState.Unavailable(message) =>
                div(
                  cls := "camera-status",
                  p(cls := "error", message),
                  button(cls := "back-button", typ := "button", "Try again", onClick --> (_ => startCamera()))
                )
          ),
          Readouts.reading(repCount.signal.map(_.toString), "reps"),
          Readouts.controls(statusText, () => resetCount(), signalMargin.signal),
          Menu.sheet(
            menuOpen,
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
            // Only where there is something to move. A menu entry that opens an empty sheet is worse than no entry,
            // and on Safari there is nothing to move: it reports exposure as modes, with no range to put a slider on.
            child <-- adjustables.signal.map: available =>
              if available.isEmpty then emptyNode
              else
                menuItem(
                  "Camera controls",
                  () => adjustOpen.set(true)
                )
            ,
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
                onClick --> (_ => captureTrace())
              )
            ,
            menuItem("Settings", () => settingsOpen.set(true)),
            menuItem("About", () => openAbout()),
            Menu.documentItems(menuOpen),
            menuItem("Back", () => show(Screen.Selection)),
            menuItem("Logout", () => logout())
          )
        ),
        // A sheet at the foot of the screen rather than a panel in the flow: what these sliders do is only visible in
        // the picture, so the picture has to stay on screen while they move.
        child <-- adjustOpen.signal
          .combineWith(adjustables.signal)
          .map: (open, available) =>
            if !open || available.isEmpty then emptyNode
            else
              val dial = stream.map(CameraAdjust.Dial(_))
              div(
                cls := "camera-sheet",
                div(
                  cls := "camera-sheet-head",
                  span("Camera"),
                  button(
                    cls := "camera-sheet-close",
                    typ := "button",
                    aria.label := "Close camera controls",
                    "\u2715",
                    onClick --> (_ => adjustOpen.set(false))
                  )
                ),
                available.map: control =>
                  val shown = Var(control.current)
                  div(
                    cls := "camera-adjust-row",
                    label(cls := "camera-adjust-label", control.label),
                    input(
                      cls := "camera-adjust-slider",
                      typ := "range",
                      minAttr := "0",
                      maxAttr := CameraAdjust.Positions.toString,
                      stepAttr := "1",
                      defaultValue := (CameraAdjust
                        .positionOf(control, control.current) * CameraAdjust.Positions).round.toString,
                      onInput.mapToValue --> { raw =>
                        raw.toDoubleOption.foreach: position =>
                          val value = CameraAdjust.valueAt(control, position / CameraAdjust.Positions)
                          shown.set(value)
                          dial.foreach(_.set(control, value))
                      }
                    ),
                    span(cls := "camera-adjust-value", child.text <-- shown.signal.map(value => f"$value%.4g"))
                  )
                ,
                // The way back from a picture no slider can recover, which is how this was first met.
                button(
                  cls := "camera-sheet-auto",
                  typ := "button",
                  "Back to automatic",
                  onClick --> { _ =>
                    dial.foreach: one =>
                      val _ = one.automatic()
                    adjustOpen.set(false)
                  }
                )
              )
        ,
        // Below the panel and laid out as the quadrants themselves are, so a trace sits where the movement that
        // produced it appeared on screen.
        div(
          cls := "signal-graphs",
          Seq(Quadrant.Q2, Quadrant.Q1, Quadrant.Q3, Quadrant.Q4).map(graphPane)
        )
      )

    val app =
      div(
        cls := "app",
        // The bench and the dashboard are the two screens not held to a hand's width: the bench frames what a camera
        // on a tripod sees, and the dashboard is read across a room from a tablet that may be either way up. Every
        // other view is deliberately kept to a phone's frame, which is the device it is actually used on.
        cls("wide") <-- stateStore.signal
          .map(Shell.of)
          .distinct
          .map:
            case Shell.SignedIn(_, Screen.Bench)     => true
            case Shell.SignedIn(_, Screen.Dashboard) => true
            case _                                   => false
        ,
        child <-- stateStore.signal
          .map(Shell.of)
          .distinct
          .map:
            // Both keep their own About and Logout in the menu, so the global bar would only duplicate them.
            case Shell.SignedIn(_, Screen.Acquirer)  => emptyNode
            case Shell.SignedIn(_, Screen.Dashboard) => emptyNode
            case Shell.SignedIn(_, _)                => userActions
            // Reachable before signing in as well. Which build a browser is running is exactly the thing one wants
            // to check on a device that will not behave, and being signed out is no reason not to be able to look.
            case Shell.Login | Shell.AuthenticationFailed(_) => guestMenu
            case _                                           => emptyNode,
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
                div(
                  cls := "home",
                  a(cls := "login-button", href := "/auth/login", "Login with Google")
                )
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
          // The account travels with the panel: a display name does not say which of several accounts this device
          // is signed into, and on a bench with four phones that is the thing one actually needs to know.
          .map(state => (state.aboutState, state.user))
          .map:
            case (AboutState.Closed, _) => emptyNode
            case (state, user)          =>
              val account = user match
                case SignedIn(email, _)  => Some(email)
                case Restoring(email, _) => Some(email)
                case _                   => None
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
                    dd(information.scalaJsVersion),
                    // What the browser is actually running, which need not be what the server just served. Stamped
                    // into the bundle at packaging time and read back out of this browser's own storage, so a cached
                    // build says so plainly instead of being mistaken for the detector misbehaving.
                    dt("Frontend build"),
                    dd(Device.frontendBuild().getOrElse("not recorded by this browser"))
                  )
                // Before signing in: the one field that can be known without a backend, which is also the one that
                // matters on a device that will not behave.
                case AboutState.LocalOnly =>
                  dl(
                    cls := "about-details",
                    dt("Frontend build"),
                    dd(Device.frontendBuild().getOrElse("not recorded by this browser"))
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
                  // Shown at once rather than with the build information, which is fetched: the account is known
                  // locally, and waiting on a request to say who is signed in would be backwards.
                  account
                    .fold(emptyNode)(email => dl(cls := "about-details about-account", dt("Signed in as"), dd(email))),
                  content
                )
              )
        ,
        child <-- settingsOpen.signal
          .map:
            case false => emptyNode
            case true  => settingsPanel()
        ,
        child <-- takeoverPending.signal
          .map:
            case false => emptyNode
            case true  =>
              div(
                cls := "about-overlay",
                div(
                  cls := "about-dialog takeover-dialog",
                  role := "alertdialog",
                  div(cls := "about-header", h2("Another device is counting")),
                  p(
                    "This account already has a device counting. Only one can, so taking over will send the other " +
                      "one back to its home screen and release its camera."
                  ),
                  div(
                    cls := "takeover-actions",
                    button(
                      cls := "mode-button",
                      typ := "button",
                      "Take over",
                      onClick --> { _ =>
                        takeoverPending.set(false)
                        show(Screen.Acquirer)
                      }
                    ),
                    button(
                      cls := "back-button",
                      typ := "button",
                      "Leave it alone",
                      onClick --> (_ => takeoverPending.set(false))
                    )
                  )
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
