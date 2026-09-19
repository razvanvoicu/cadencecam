package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.{AccountSettings, Workout}
import sgrv.fe.ApiClient.ApiFailure

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.Failure
import scala.util.Success

/** The account's workouts, newest first, and the throwing away of any one of them.
  *
  * Read when the panel opens rather than kept: a history is looked at occasionally and changes rarely, and a copy held
  * from the last time it was opened would show a workout that has since been deleted from another device.
  */
private[fe] final class HistoryPanel(api: ApiClient, accountSettings: Var[AccountSettings]):
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

    def close(): Unit = isOpen.set(false)

    def load(): Unit =
      api.workoutHistory().onComplete:
        case Success(history) =>
          failed.set(None)
          workouts.set(Some(history.workouts))
        case Failure(error) =>
          failed.set(Some(errorMessage(error)))
          workouts.set(Some(Seq.empty))

    /** Removes one workout, and takes it off the list without asking for the whole history again. */
    def discard(workout: Workout): Unit =
      confirming.set(None)
      api.discardWorkout(workout.id).onComplete:
        case Success(_) =>
          failed.set(None)
          workouts.update(_.map(_.filterNot(_.id == workout.id)))
        case Failure(ApiFailure(_, status)) =>
          failed.set(Some(s"That workout could not be deleted (HTTP $status)."))
        case Failure(error) => failed.set(Some(errorMessage(error)))

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
        val document =
          WorkoutCsv.of(listed, accountSettings.now(), millis => new js.Date(millis).toISOString())
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
        div(
          cls := "workout-when",
          span(cls := "workout-date", child.text <-- updates.map(workout => when(workout.startedAtMillis))),
          child <-- updates.map: workout =>
            if workout.running then span(cls := "workout-state", "in progress")
            else span(cls := "workout-state", Effort.elapsedClock(workout.elapsedSeconds))
        ),
        div(
          cls := "workout-figures",
          span(
            cls := "workout-figure",
            child.text <-- updates.map(_.reps.toString),
            span(cls := "workout-unit", "reps")
          ),
          span(
            cls := "workout-figure",
            child.text <-- updates
              .combineWith(accountSettings.signal)
              .map: (workout, settings) =>
                Effort.grouped(Effort.calories(workout.reps, workout.cadenceSum, settings)),
            span(cls := "workout-unit", "cal")
          )
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
              "\u00d7",
              onClick --> (_ => confirming.set(Some(id)))
            )
      )

    div(
      cls := "settings-overlay",
      onMountCallback(_ => load()),
      div(
        cls := "settings-screen history-screen",
        role := "dialog",
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
    )
