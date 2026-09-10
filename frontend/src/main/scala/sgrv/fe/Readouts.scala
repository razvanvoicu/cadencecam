package sgrv.fe

import com.raquo.laminar.api.L.*

/** The rows the acquirer and the dashboard both show.
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

  /** The reset control, the status line, and a counterweight that keeps the line centred on the panel.
    *
    * The spacer matches the control's footprint exactly. Without it the line would centre on the space left over
    * beside the button, which is not the middle of anything the eye can see.
    */
  def controls(status: Signal[String], onReset: () => Unit): HtmlElement =
    div(
      cls := "control-row",
      button(
        cls := "reset-button",
        typ := "button",
        // U+21BA, the anticlockwise open circle arrow: monochrome, present in the system fonts of every platform
        // this runs on, and unambiguous without a caption.
        "↺",
        aria.label := "Reset the count",
        title := "Reset the count",
        onClick --> (_ => onReset())
      ),
      // Says which of the three it is doing rather than letting a stalled count look like a steady one.
      p(cls := "lock-state", child.text <-- status),
      // Balances the reset control on the other side. Hidden from assistive technology: it carries nothing to say.
      div(cls := "footer-spacer", aria.hidden := true)
    )

  /** A pace, in whole reps or calories per minute.
    *
    * Rounded rather than given to a decimal: the figure moves with every rep, and a trailing digit that changes
    * constantly reads as noise rather than as information.
    */
  def perMinute(value: Signal[Double]): Signal[String] = value.map(rate => math.round(rate).toString)
