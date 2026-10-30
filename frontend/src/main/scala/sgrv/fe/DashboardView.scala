package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.LiveCommand

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js

/** The dashboard that watches whichever device is acquiring for this account.
  *
  * It holds no count of its own. The raw measurement is a copy of what the acquirer last said, while this dashboard
  * resolves the shared exercise id against its own settings and calculates calories locally. Start asks the counter to
  * open the workout; Stop freezes this dashboard's own conclusion and writes it directly to history before asking the
  * counter only to halt.
  */
private[fe] object DashboardView:
  def apply(
      api: ApiClient,
      settingsPanel: SettingsPanel,
      historyPanel: HistoryPanel,
      storage: dom.Storage,
      show: Screen => Unit,
      openAbout: () => Unit,
      logoutThisDevice: () => Unit,
      logoutAllDevices: () => Unit,
      confirmForget: () => Unit
  ): Element =
    val controller = DashboardController(api.http, settingsPanel.settings.now().selected)
    val menuOpen = Var(false)
    val workoutBusy = Var(false)
    val workoutFailure = Var(Option.empty[String])
    val workoutRequests = RequestScope()

    /** Whether this device opens on the dashboard. Mirrors what is stored, so the menu can say which way the next tap
      * turns it.
      */
    val defaultsToDashboard = Var(StartPreference.defaultsToDashboard(storage))

    /** Everything on the screen is a function of the reading, the window and the account's settings, so the three are
      * combined once and every figure below is read off the result. Separately derived signals would each re-evaluate
      * the same arithmetic and, worse, could disagree about which reading they were describing.
      */
    val selectedSettings = settingsPanel.settings.signal
      .combineWith(controller.exerciseId)
      .map((settings, exerciseId) => DashboardWorkout.selectedSettings(settings, exerciseId))

    val figures = controller.reading
      .combineWith(controller.window.signal, selectedSettings)
      .map: (latest, marks, settings) =>
        val counted = latest.fold(0)(_.reps)
        val elapsedMinutes = latest.fold(0.0)(_.elapsedSeconds) / 60.0
        val cadenceSum = latest.fold(0.0)(_.cadenceSum)
        val calories = settings.fold(0.0)(Effort.calories(counted, cadenceSum, _))
        val burn = settings.flatMap(Effort.burn(marks, _))
        (counted, elapsedMinutes, calories, Effort.pace(marks), burn)

    val boundary =
      controller.elapsedSeconds.map(seconds => Effort.boundaryLabel(Effort.boundaryMinutes(seconds / 60.0)))

    def figure(
        of: ((Int, Double, Double, Option[Double], Option[Double])) => Option[Double],
        showValue: Double => String
    ): Signal[String] =
      figures.map(values => of(values).fold(Effort.Absent)(showValue))

    val repsPerMinute = figure((_, _, _, pace, _) => pace, Effort.decimal)
    val repsAtBoundary =
      figure(
        (reps, minutes, _, pace, _) => pace.flatMap(Effort.atBoundary(reps.toDouble, _, minutes)),
        Effort.decimal
      )
    val calories = figures.map((_, _, value, _, _) => Effort.decimal(value))
    val caloriesPerMinute = figure((_, _, _, _, burn) => burn, Effort.decimal)
    val caloriesAtBoundary =
      figure(
        (_, minutes, value, _, burn) => burn.flatMap(Effort.atBoundary(value, _, minutes)),
        Effort.decimal
      )

    def menuItem(label: String, act: () => Unit): Element = Menu.item(menuOpen, label, act)

    var typographyFrame = Option.empty[Int]
    var typographyRoot = Option.empty[dom.html.Element]

    def scheduleTypography(): Unit =
      typographyFrame.foreach(dom.window.cancelAnimationFrame)
      typographyFrame = Some(
        dom.window.requestAnimationFrame: _ =>
          typographyFrame = None
          typographyRoot.foreach(DashboardTypography.fit)
      )

    val resizeListener: js.Function1[dom.Event, Unit] = _ => scheduleTypography()

    val typographyInputs = figures
      .combineWith(boundary, selectedSettings)
      .combineWith(controller.elapsedSeconds)

    val shownStatus = controller.status
      .combineWith(workoutFailure.signal)
      .map((status, failure) => failure.getOrElse(status))

    /** Freezes this dashboard's reading and settings before starting either asynchronous action. */
    def stopWorkout(): Unit =
      if !workoutBusy.now() then
        val command = for
          reading <- controller.currentReading.filter(_.active)
          stop <- DashboardWorkout.stop(
            reading,
            settingsPanel.settings.now(),
            controller.currentExerciseId
          )
        yield stop
        command match
          case None =>
            workoutFailure.set(Some("The selected exercise is not available on this dashboard."))
          case Some(stop) =>
            workoutBusy.set(true)
            workoutFailure.set(None)
            workoutRequests.run(api.controlWorkout(stop.action, stop.progress, stop.snapshot)):
              case Right(state) if !state.active =>
                workoutBusy.set(false)
                // The completed snapshot is already in history. This command contains no calories; it only makes the
                // counter stop acquiring and reporting into the workout which has just closed.
                controller.ask(LiveCommand.DashboardStopped)
              case Right(_) =>
                workoutBusy.set(false)
                workoutFailure.set(Some("The workout is still active. Try Stop again."))
              case Left(error) =>
                workoutBusy.set(false)
                workoutFailure.set(Some(error.message))

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

    // Laid out as a column of rows sized by their content, with the panel taking what is left. Nothing is positioned
    // absolutely and no row has a fixed height, so a landscape arrangement later is a change of direction on the
    // container rather than a rewrite.
    lazy val dashboardRoot: HtmlElement = div(
      cls := "dashboard-view",
      typographyInputs --> (_ => scheduleTypography()),
      settingsPanel.settings.signal.map(_.selected).distinct --> controller.chooseExercise,
      onMountCallback { _ =>
        typographyRoot = Some(dashboardRoot.ref)
        controller.connect()
        // Read again on arrival as well as at login: a tablet left on this screen overnight is signed in from a session
        // it has not re-established, and the settings may have been changed from the phone in the meantime.
        settingsPanel.load()
        dom.window.addEventListener("resize", resizeListener)
        scheduleTypography()
      },
      onUnmountCallback { _ =>
        workoutRequests.invalidate()
        controller.close()
        dom.window.removeEventListener("resize", resizeListener)
        typographyFrame.foreach(dom.window.cancelAnimationFrame)
        typographyFrame = None
        typographyRoot = None
      },
      div(
        cls := "screen dashboard",
        div(
          cls := "dashboard-header",
          div(
            cls := "dashboard-title",
            h1(
              cls := "screen-title",
              child.text <-- selectedSettings.map:
                case Some(settings) => settings.selectedExercise.fold("Counting")(_.name)
                case None           => "Exercise unavailable"
            ),
            span(
              cls := "dashboard-basis",
              child.text <-- selectedSettings.map:
                case Some(settings) =>
                  s"factor ${Effort.factorText(settings.factor)} · ${Effort.modeText(settings.countsBy)}"
                case None => "settings out of date"
            )
          ),
          div(
            cls := "dashboard-clock",
            span(cls := "clock-value", child.text <-- controller.elapsedSeconds.map(Effort.elapsedClock)),
            Menu.toggle(menuOpen)
          )
        ),
        Menu.backdrop(menuOpen),
        div(
          cls := "figure-cards",
          card(
            "reps-card",
            "Reps",
            controller.reps.map(reps => Effort.grouped(reps.toDouble)),
            repsPerMinute,
            repsAtBoundary
          ),
          card("calories-card", "Calories", calories, caloriesPerMinute, caloriesAtBoundary)
        ),
        div(
          cls := "dashboard-status",
          child <-- controller.workoutActive
            .combineWith(workoutBusy.signal)
            .map: (active, busy) =>
              val label = if active then "Stop workout" else "Start workout"
              button(
                cls := "workout-button",
                typ := "button",
                disabled := busy,
                if active then "■" else "▶",
                aria.label := label,
                title := label,
                onClick --> (_ => if active then stopWorkout() else controller.ask(LiveCommand.Start))
              )
          ,
          p(cls := "lock-state", child.text <-- shownStatus)
        ),
        Menu.sheet(
          menuOpen,
          menuItem("Settings", () => settingsPanel.open()),
          menuItem("History", () => historyPanel.open()),
          Menu.item(
            menuOpen,
            defaultsToDashboard.signal.map(on => if on then "Default to dashboard off" else "Default to dashboard on"),
            () =>
              val on = !defaultsToDashboard.now()
              StartPreference.setDefaultsToDashboard(storage, on)
              defaultsToDashboard.set(on)
          ),
          menuItem("Delete all my data", confirmForget),
          menuItem("Capture signal trace", () => controller.ask(LiveCommand.CaptureTrace())),
          menuItem("About", openAbout),
          Menu.documentItems(menuOpen),
          menuItem("Back", () => show(Screen.Selection)),
          menuItem("Log out this device only", logoutThisDevice),
          menuItem("Log out all devices", logoutAllDevices)
        )
      )
    )

    dashboardRoot
