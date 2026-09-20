package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.Workout
import sgrv.fe.ApiClient.ApiError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js

/** The account's workouts, newest first, and the throwing away of any one of them.
  *
  * Read when the panel opens rather than kept: a history is looked at occasionally and changes rarely, and a copy held
  * from the last time it was opened would show a workout that has since been deleted from another device.
  */
private[fe] final class HistoryPanel(api: ApiClient):
  /** A panel over the dashboard rather than a screen of its own, so opening it does not tear down the link to the
    * counting device and make it pair again on the way back.
    */
  val isOpen: Var[Boolean] = Var(false)

  def open(): Unit = isOpen.set(true)

  def view(): Element =
    val workouts = Var(Option.empty[Seq[Workout]])
    val failed = Var(Option.empty[String])
    val confirming = Var(Option.empty[String])
    val menuOpen = Var(false)
    val loadRequests = RequestScope()
    val discardRequests = RequestScope()
    var typographyFrame = Option.empty[Int]
    var typographyRoot = Option.empty[dom.html.Element]

    def scheduleTypography(): Unit =
      typographyFrame.foreach(dom.window.cancelAnimationFrame)
      typographyFrame = Some(
        dom.window.requestAnimationFrame: _ =>
          typographyFrame = None
          typographyRoot.foreach(HistoryTypography.fit)
      )

    val resizeListener: js.Function1[dom.Event, Unit] = _ => scheduleTypography()

    def stopRequests(): Unit =
      loadRequests.invalidate()
      discardRequests.invalidate()

    def close(): Unit =
      stopRequests()
      isOpen.set(false)

    def load(): Unit =
      loadRequests.latest(api.workoutHistory()):
        case Right(history) =>
          failed.set(None)
          workouts.set(Some(history.workouts))
        case Left(error) =>
          failed.set(Some(error.message))
          workouts.set(Some(Seq.empty))

    /** Removes one workout, and takes it off the list without asking for the whole history again. */
    def discard(workout: Workout): Unit =
      confirming.set(None)
      discardRequests.run(api.discardWorkout(workout.id)):
        case Right(_) =>
          failed.set(None)
          workouts.update(_.map(_.filterNot(_.id == workout.id)))
        case Left(ApiError.Http(_, status, _)) =>
          failed.set(Some(s"That workout could not be deleted (HTTP $status)."))
        case Left(error) => failed.set(Some(error.message))

    /** Hands the history to the browser as a file to save.
      *
      * Built here from what is already on screen rather than asked of the server: the list has been fetched, and a
      * second route returning the same rows in a second format is a second thing to keep in step with the first.
      */
    def exportCsv(): Unit =
      val listed = workouts.now().getOrElse(Seq.empty)
      if listed.isEmpty then failed.set(Some("There is nothing to export yet."))
      else
        failed.set(None)
        val document = WorkoutCsv.of(listed, millis => new js.Date(millis).toISOString())
        val blob = dom.Blob(js.Array(document), new dom.BlobPropertyBag { `type` = "text/csv;charset=utf-8" })
        val address = dom.URL.createObjectURL(blob)
        val anchor = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
        anchor.href = address
        anchor.setAttribute("download", WorkoutCsv.FileName)
        // Attached before the click and taken away after: a detached anchor's click is ignored by some browsers, and
        // one left behind would accumulate a node per export.
        val _ = dom.document.body.appendChild(anchor)
        anchor.click()
        val _ = dom.document.body.removeChild(anchor)
        dom.URL.revokeObjectURL(address)

    /** When a workout happened, in the reader's own locale: this is their day being named back to them, and the
      * conventions for that are theirs rather than this app's.
      */
    def when(millis: Double): String =
      val at = new js.Date(millis)
      s"${at.toLocaleDateString()} ${at.toLocaleTimeString()}"

    def row(id: String, initial: Workout, updates: Signal[Workout]): Element =
      div(
        cls := "workout-row",
        cls("running") <-- updates.map(_.running),
        span(cls := "workout-date", child.text <-- updates.map(workout => when(workout.startedAtMillis))),
        div(
          cls := "workout-figures",
          span(
            cls := "workout-figure",
            span(cls := "workout-amount", child.text <-- updates.map(_.reps.toString)),
            span(cls := "workout-unit", "reps")
          ),
          span(
            cls := "workout-figure",
            span(
              cls := "workout-amount",
              child.text <-- updates
                .map(_.snapshot.fold(Effort.Absent)(saved => Effort.decimal(saved.calories)))
            ),
            span(cls := "workout-unit", "cal")
          )
        ),
        span(
          cls := "workout-duration",
          child.text <-- updates.map: workout =>
            val duration = if workout.running then "in progress" else Effort.elapsedClock(workout.elapsedSeconds)
            s"Duration: $duration"
        ),
        span(
          cls := "workout-meta",
          child.text <-- updates.map: workout =>
            val exercise = workout.snapshot.fold("Exercise unavailable")(_.exerciseType)
            val factor =
              workout.snapshot.fold("factor unavailable")(saved => s"factor ${Effort.factorText(saved.exerciseFactor)}")
            s"$exercise · $factor"
        ),
        // Two taps, because this is the one control on the screen that destroys something. The confirmation is the row
        // itself rather than a dialog: what is about to go is the thing being pointed at.
        child <-- confirming.signal.map: asked =>
          if asked.contains(id) then
            div(
              cls := "workout-confirm",
              button(cls := "workout-yes", typ := "button", "Delete", onClick --> (_ => discard(initial))),
              button(cls := "workout-no", typ := "button", "Keep", onClick --> (_ => confirming.set(None)))
            )
          else
            button(
              cls := "workout-remove",
              typ := "button",
              aria.label := "Delete this workout",
              title := "Delete this workout",
              svg.svg(
                svg.cls := "workout-remove-icon",
                svg.viewBox := "0 0 24 24",
                aria.hidden := true,
                svg.path(svg.d := "M3 6h18 M8 6V4h8v2 M19 6l-1 14H6L5 6 M10 10v6 M14 10v6")
              ),
              onClick --> (_ => confirming.set(Some(id)))
            )
      )

    lazy val historyScreen: HtmlElement = div(
      cls := "settings-screen history-screen",
      role := "dialog",
      workouts.signal --> (_ => scheduleTypography()),
      onMountCallback { _ =>
        typographyRoot = Some(historyScreen.ref)
        dom.window.addEventListener("resize", resizeListener)
        scheduleTypography()
      },
      div(
        cls := "settings-header",
        h2("History"),
        span(
          cls := "settings-note",
          child.text <-- workouts.signal.map:
            case None         => "reading…"
            case Some(listed) => if listed.isEmpty then "" else s"${listed.size} workouts"
        ),
        Menu.toggle(menuOpen),
        Menu.backdrop(menuOpen),
        Menu.sheet(
          menuOpen,
          Menu.item(menuOpen, "Export as CSV", () => exportCsv()),
          Menu.documentItems(menuOpen)
        ),
        button(
          cls := "about-close",
          typ := "button",
          title := "Close",
          onClick --> (_ => close()),
          "\u00d7"
        )
      ),
      child <-- failed.signal.map:
        case None          => emptyNode
        case Some(message) => p(cls := "error settings-error", message)
      ,
      div(
        cls := "settings-body history-body",
        child <-- workouts.signal.map:
          case None                           => p(cls := "field-hint", "Reading your history…")
          case Some(listed) if listed.isEmpty =>
            p(cls := "field-hint", "Nothing counted yet. Your workouts will appear here.")
          case Some(_) => emptyNode
        ,
        div(
          cls := "workout-list",
          children <-- workouts.signal.map(_.getOrElse(Seq.empty)).split(_.id)(row)
        )
      ),
      div(
        cls := "settings-actions",
        button(cls := "back-button", typ := "button", "Close", onClick --> (_ => close()))
      )
    )

    div(
      cls := "settings-overlay",
      onMountCallback(_ => load()),
      onUnmountCallback { _ =>
        stopRequests()
        dom.window.removeEventListener("resize", resizeListener)
        typographyFrame.foreach(dom.window.cancelAnimationFrame)
        typographyFrame = None
        typographyRoot = None
      },
      historyScreen
    )
