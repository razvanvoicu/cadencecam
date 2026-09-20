package sgrv.fe

import com.raquo.laminar.api.L.*
import sgrv.fe.acquire.SignalStrength

/** The rows the counter and the bench both show: the large reading, and the workout control beside the status line.
  *
  * Shared as builders rather than duplicated as markup, because "exactly the same format" is a promise that copies
  * cannot keep: the two would agree on the day they were written and drift on the first change to either. Built from
  * one place, they cannot disagree at all.
  *
  * Each is a row in a column, sized by its content, with no fixed heights or absolute positioning — so a later
  * landscape layout can put these side by side by changing the container's direction and nothing else.
  */
private[fe] object Readouts:

  /** A large number with its unit beside it: the reading these screens exist to show.
    *
    * The number is centred on the panel rather than the number-and-unit pair being centred, so it stays put as the
    * count grows from one digit to three. Three columns with equal flexible outer ones do that; the unit sits in the
    * right one and the middle one lands on the centre.
    */
  def reading(value: Signal[String], unit: String, extra: Modifier[HtmlElement]*): HtmlElement =
    div(
      cls := "rep-count",
      span(cls := "rep-count-value", child.text <-- value),
      span(cls := "rep-count-label", unit),
      extra
    )

  /** The start/stop control, the status line, and a counterweight that keeps the line centred on the panel.
    *
    * The spacer matches the control's footprint exactly. Without it the line would centre on the space left over beside
    * the button, which is not the middle of anything the eye can see.
    */
  def controls(
      status: Signal[String],
      active: Signal[Boolean],
      busy: Signal[Boolean],
      onStart: () => Unit,
      onStop: () => Unit,
      // Absent on a watching screen, which has no camera of its own to judge.
      margin: Signal[Option[Double]] = Val(None)
  ): HtmlElement =
    div(
      cls := "control-row",
      child <-- active
        .combineWith(busy)
        .map: (isActive, isBusy) =>
          val label = if isActive then "Stop workout" else "Start workout"
          button(
            cls := "workout-button",
            typ := "button",
            disabled := isBusy,
            // U+25B6 and U+25A0: monochrome transport controls whose meaning survives without a word inside the
            // compact circular button. The accessible name carries that word for assistive technology.
            if isActive then "■" else "▶",
            aria.label := label,
            title := label,
            onClick --> (_ => if isActive then onStop() else onStart())
          )
      ,
      // Says which of the three it is doing rather than letting a stalled count look like a steady one.
      p(cls := "lock-state", child.text <-- status),
      // One gauge in the counterweight's column: how far the movement stands above the background. The noise reading
      // that sat beside it is gone. It measured something real, but the two were read as a pair of scores to be got
      // right rather than as one question with one remedy, and the remedy for both is the same -- get more movement
      // into the frame than there is background.
      div(
        cls := "gauge signal-badge",
        cls("strong") <-- margin.map(_.exists(SignalStrength.of(_) == SignalStrength.Strong)),
        cls("adequate") <-- margin.map(_.exists(SignalStrength.of(_) == SignalStrength.Adequate)),
        cls("weak") <-- margin.map(_.exists(SignalStrength.of(_) == SignalStrength.Weak)),
        child.text <-- margin.map(_.fold("")(SignalStrength.label)),
        title <-- margin.map(_.fold("")(m => s"${SignalStrength.label(m)}: ${SignalStrength.Description}")),
        aria.label <-- margin.map(
          _.fold("")(m => s"Signal ${SignalStrength.label(m)}, ${SignalStrength.Description}")
        )
      )
    )
