package sgrv.fe

import org.scalajs.dom

/** Fits every history row from one shared type size.
  *
  * A timestamp, the figures and the duration must remain peers: independently viewport-sized text made one of them
  * dominate another as the panel narrowed. They are measured together at one known size and assigned the smallest fit
  * that keeps every line inside the eighty-percent content allowance of its grid track. The descriptive exercise line
  * follows a smaller ratio with a modest legibility floor, so it remains secondary without drifting independently.
  */
private[fe] object HistoryTypography:
  // A long exercise name should ellipsize rather than making every important
  // number in the history row tiny, so metadata is not part of the shared fit.
  private val ItemSelector = ".workout-date, .workout-figures, .workout-duration"
  private val MeasuringSizePixels = 10.0
  private val BreathingRoom = 0.96
  private val SecondaryRatio = 0.8
  private val MinimumPixels = 9.5
  private val MinimumSecondaryPixels = 9.0
  private val MaximumPixels = 20.0

  def fit(root: dom.html.Element): Unit =
    setSizes(root, MeasuringSizePixels)
    val fitted = smallestFit(root, ItemSelector)
    setSizes(root, legible(fitted))

  private def setSizes(root: dom.html.Element, primaryPixels: Double): Unit =
    root.style.setProperty("--history-primary-size", cssPixels(primaryPixels))
    val secondaryPixels = math.max(MinimumSecondaryPixels, primaryPixels * SecondaryRatio)
    root.style.setProperty("--history-secondary-size", cssPixels(secondaryPixels))

  private def smallestFit(root: dom.html.Element, selector: String): Double =
    val elements = root.querySelectorAll(selector)
    (0 until elements.length)
      .flatMap: index =>
        val element = elements.item(index).asInstanceOf[dom.html.Element]
        val available = element.getBoundingClientRect().width
        val range = dom.document.createRange()
        range.selectNodeContents(element)
        val rendered = range.getBoundingClientRect().width
        fittedPixels(available, rendered)
      .minOption
      .getOrElse(MaximumPixels)

  private[fe] def fittedPixels(availablePixels: Double, renderedAtMeasuringSizePixels: Double): Option[Double] =
    Option.when(availablePixels > 0 && renderedAtMeasuringSizePixels > 0):
      MeasuringSizePixels * availablePixels / renderedAtMeasuringSizePixels * BreathingRoom

  private[fe] def legible(value: Double): Double =
    math.max(MinimumPixels, math.min(MaximumPixels, value))

  private[fe] def cssPixels(value: Double): String = f"${math.max(1.0, value)}%.2fpx"
