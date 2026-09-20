package sgrv.fe

import org.scalajs.dom

/** Fits the dashboard's two shared type scales to the text and space actually on screen.
  *
  * CSS can scale text with the viewport, but it cannot ask several unrelated boxes how large each label could be and
  * then give every label the smallest answer. This does exactly that. Every candidate is measured at `1vw`, so the
  * ratio between available width and rendered width is already the answer in vw and does not depend on device pixels.
  */
private[fe] object DashboardTypography:
  private val LabelSelector = ".dashboard .screen-title, .clock-value, .figure-label, .footnote-label"
  private val ValueSelector = ".figure-value, .footnote-value"
  private val MeasuringSizeVw = 1.0
  private val BreathingRoom = 0.94
  private val DetailRatio = 0.6

  def fit(root: dom.html.Element): Unit =
    root.style.setProperty("--dashboard-label-size", s"${MeasuringSizeVw}vw")
    root.style.setProperty("--dashboard-value-size", s"${MeasuringSizeVw}vw")

    val label = smallestFit(root, LabelSelector)
    val value = smallestFit(root, ValueSelector)
    root.style.setProperty("--dashboard-label-size", cssVw(label))
    root.style.setProperty("--dashboard-value-size", cssVw(value))
    root.style.setProperty("--dashboard-detail-size", cssVw(label * DetailRatio))

  private def smallestFit(root: dom.html.Element, selector: String): Double =
    val elements = root.querySelectorAll(selector)
    (0 until elements.length)
      .flatMap: index =>
        val element = elements.item(index).asInstanceOf[dom.html.Element]
        val available = element.getBoundingClientRect().width
        val range = dom.document.createRange()
        range.selectNodeContents(element)
        val rendered = range.getBoundingClientRect().width
        fittedVw(available, rendered)
      .minOption
      .getOrElse(MeasuringSizeVw)

  private[fe] def fittedVw(availablePixels: Double, renderedAtOneVwPixels: Double): Option[Double] =
    Option.when(availablePixels > 0 && renderedAtOneVwPixels > 0):
      MeasuringSizeVw * availablePixels / renderedAtOneVwPixels * BreathingRoom

  private[fe] def cssVw(value: Double): String =
    f"${math.max(0.01, value)}%.4fvw"
