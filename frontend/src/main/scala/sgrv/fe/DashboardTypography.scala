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
  private val SecondaryValueSelector = ".footnote-value"
  private val MeasuringSizeVw = 1.0
  private val BreathingRoom = 0.94
  private val DetailRatio = 0.6
  private val LabelCardHeightRatio = 0.12
  private val SecondaryValueCardHeightRatio = 0.14

  def fit(root: dom.html.Element): Unit =
    root.style.setProperty("--dashboard-label-size", s"${MeasuringSizeVw}vw")
    root.style.setProperty("--dashboard-secondary-value-size", s"${MeasuringSizeVw}vw")

    val cardHeight = smallestCardHeight(root)
    val viewportWidth = dom.window.innerWidth
    val labelFit = smallestFit(root, LabelSelector)
    val secondaryValueFit = smallestFit(root, SecondaryValueSelector)
    val label = heightLimitVw(cardHeight, viewportWidth, LabelCardHeightRatio)
      .fold(labelFit)(labelFit.min)
    val secondaryValue = heightLimitVw(cardHeight, viewportWidth, SecondaryValueCardHeightRatio)
      .fold(secondaryValueFit)(secondaryValueFit.min)
    root.style.setProperty("--dashboard-label-size", cssVw(label))
    root.style.setProperty("--dashboard-secondary-value-size", cssVw(secondaryValue))
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

  private def smallestCardHeight(root: dom.html.Element): Double =
    val cards = root.querySelectorAll(".figure-card")
    (0 until cards.length)
      .map(index => cards.item(index).asInstanceOf[dom.html.Element].getBoundingClientRect().height)
      .filter(_ > 0)
      .minOption
      .getOrElse(0.0)

  private[fe] def fittedVw(availablePixels: Double, renderedAtOneVwPixels: Double): Option[Double] =
    Option.when(availablePixels > 0 && renderedAtOneVwPixels > 0):
      MeasuringSizeVw * availablePixels / renderedAtOneVwPixels * BreathingRoom

  private[fe] def heightLimitVw(cardHeightPixels: Double, viewportWidthPixels: Double, ratio: Double): Option[Double] =
    Option.when(cardHeightPixels > 0 && viewportWidthPixels > 0 && ratio > 0):
      cardHeightPixels * ratio / viewportWidthPixels * 100.0

  private[fe] def cssVw(value: Double): String =
    f"${math.max(0.01, value)}%.4fvw"
