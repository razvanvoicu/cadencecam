package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.{CountsBy, LiveCommand}

/** The dashboard that watches whichever device is acquiring for this account.
  *
  * It holds no count of its own. Everything shown here is a copy of what the acquirer last said, and the two controls
  * ask that device to do the things its own controls do rather than acting locally — so the acquirer stays the single
  * authority on the tally, and a dashboard that has been away simply catches up.
  */
private[fe] object DashboardView:
  def apply(
      api: ApiClient,
      settingsPanel: SettingsPanel,
      historyPanel: HistoryPanel,
      storage: dom.Storage,
      show: Screen => Unit,
      openAbout: () => Unit,
      logout: () => Unit,
      confirmForget: () => Unit
  ): Element =
    val controller = DashboardController(api.http)
    val menuOpen = Var(false)

    /** Whether this device opens on the dashboard. Mirrors what is stored, so the menu can say which way the next tap
      * turns it.
      */
    val defaultsToDashboard = Var(StartPreference.defaultsToDashboard(storage))

    /** Everything on the screen is a function of the reading, the window and the account's settings, so the three are
      * combined once and every figure below is read off the result. Separately derived signals would each re-evaluate
      * the same arithmetic and, worse, could disagree about which reading they were describing.
      */
    val figures = controller.reading
      .combineWith(controller.window.signal, settingsPanel.settings.signal)
      .map: (latest, marks, settings) =>
        val counted = latest.fold(0)(_.reps)
        val elapsedMinutes = latest.fold(0.0)(_.elapsedSeconds) / 60.0
        val calories = Effort.calories(counted, latest.fold(0.0)(_.cadenceSum), settings)
        (counted, elapsedMinutes, calories, Effort.pace(marks), Effort.burn(marks, settings))

    val boundary =
      controller.elapsedSeconds.map(seconds => Effort.boundaryLabel(Effort.boundaryMinutes(seconds / 60.0)))

    def figure(
        of: ((Int, Double, Double, Option[Double], Option[Double])) => Option[Double],
        showValue: Double => String
    ): Signal[String] =
      figures.map(values => of(values).fold(Effort.Absent)(showValue))

    val repsPerMinute = figure((_, _, _, pace, _) => pace, Effort.rate)
    val repsAtBoundary =
      figure(
        (reps, minutes, _, pace, _) => pace.flatMap(Effort.atBoundary(reps.toDouble, _, minutes)),
        Effort.grouped
      )
    val calories = figures.map((_, _, value, _, _) => Effort.grouped(value))
    val caloriesPerMinute = figure((_, _, _, _, burn) => burn, Effort.rate)
    val caloriesAtBoundary =
      figure(
        (_, minutes, value, _, burn) => burn.flatMap(Effort.atBoundary(value, _, minutes)),
        Effort.grouped
      )

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

    // Laid out as a column of rows sized by their content, with the panel taking what is left. Nothing is positioned
    // absolutely and no row has a fixed height, so a landscape arrangement later is a change of direction on the
    // container rather than a rewrite.
    div(
      cls := "dashboard-view",
      onMountCallback { _ =>
        controller.connect()
        // Read again on arrival as well as at login: a tablet left on this screen overnight is signed in from a session
        // it has not re-established, and the settings may have been changed from the phone in the meantime.
        settingsPanel.load()
      },
      onUnmountCallback(_ => controller.close()),
      div(
        cls := "screen dashboard",
        div(
          cls := "dashboard-header",
          div(
            cls := "dashboard-title",
            h1(
              cls := "screen-title",
              child.text <-- settingsPanel.settings.signal.map(_.selectedExercise.fold("Counting")(_.name))
            ),
            span(
              cls := "dashboard-basis",
              child.text <-- settingsPanel.settings.signal.map: settings =>
                val how = settings.countsBy match
                  case CountsBy.RepCount  => "rep count"
                  case CountsBy.Frequency => "frequency"
                s"factor ${Effort.factorText(settings.factor)} · $how"
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
          card("reps-card", "Reps", controller.reps.map(_.toString), repsPerMinute, repsAtBoundary),
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
            onClick --> (_ => controller.ask(LiveCommand.Reset))
          ),
          p(cls := "lock-state", child.text <-- controller.status)
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
          menuItem("Logout", logout)
        )
      )
    )
