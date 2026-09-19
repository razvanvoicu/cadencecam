package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.{AccountSettings, CountsBy, ExerciseType, WeightUnit}
import sgrv.fe.ApiClient.ApiError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js

/** Account settings shared by the screens, plus the panel that edits them. */
private[fe] final class SettingsPanel(api: ApiClient):
  /** What this account has set, as last read from its record.
    *
    * Held outside either main screen, because more than one screen asks and because a screen that fetched its own copy
    * on every mount would show the defaults for as long as the request took -- which on the dashboard means a calorie
    * figure that is wrong first and right a moment later.
    *
    * Starts at the defaults rather than at nothing: every figure that depends on these has a sensible value from the
    * first paint, and the account's own numbers replace them when they arrive.
    */
  val settings: Var[AccountSettings] = Var(AccountSettings.Initial)

  /** Deliberately not in [[FrontendState]]: this is a panel over whatever screen is running, and persisting it would
    * reopen it on a reload -- over a camera that then keeps counting behind it.
    */
  val isOpen: Var[Boolean] = Var(false)
  private val loadRequests = RequestScope()

  def open(): Unit = isOpen.set(true)

  /** Reads the account's settings. Called when a session is confirmed, which is what "on every login" means here.
    *
    * A failure leaves the defaults in place and says so in the console only. There is nothing a person can do about it
    * from the dashboard, and refusing to draw the screen over it would turn a wrong factor into no screen at all.
    */
  def load(): Unit =
    loadRequests.latest(api.accountSettings()):
      case Right(value) => settings.set(value)
      case Left(error)  => dom.console.warn(error.message)

  /** The settings panel: a weight, a default factor, and the exercises this account counts.
    *
    * Edited against a copy and written back only on Save, so leaving by Cancel leaves nothing behind. The copy is taken
    * when the panel opens rather than held permanently, which is what makes Cancel a real undo of everything done
    * inside it.
    */
  def view(): Element =
    val draft = Var(settings.now())
    val saving = Var(false)
    val failed = Var(Option.empty[String])
    val saveRequests = RequestScope()

    def close(): Unit =
      saveRequests.invalidate()
      isOpen.set(false)

    def commit(): Unit =
      if !saving.now() then
        saving.set(true)
        failed.set(None)
        val next = draft.now()
        saveRequests.run(api.saveAccountSettings(next)): outcome =>
          saving.set(false)
          outcome match
            case Right(_) =>
              // From the draft rather than from another request: the screens behind this one should show the new
              // figures the moment it closes, not one round trip later.
              settings.set(next)
              close()
            case Left(ApiError.Http(_, status, _)) =>
              failed.set(Some(s"The server would not save these settings (HTTP $status)."))
            case Left(error) =>
              failed.set(Some(error.message))

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
      * Typed, because a factor calibrated against a real machine is a number like 0.0125 and no amount of tapping gets
      * there from one. Nudged proportionally, because the same control also has to serve a factor of two.
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
        // Redrawn on the value the buttons put there, so a nudge is reflected in the field while typing into it is not
        // fought by a redraw of what is being typed.
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
      onUnmountCallback(_ => saveRequests.invalidate()),
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
