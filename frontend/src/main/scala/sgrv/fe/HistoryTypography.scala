package sgrv.fe

import org.scalajs.dom

/** Fits every history row from a shared type size.
  *
  * On a phone, each timestamp occupies eighty percent of all the space left beside the delete icon. Its figures and
  * duration inherit that exact size, while the exercise description is sixty percent of it. On wider screens all rows
  * share the smallest primary size needed to keep the two-column layout on one line.
  */
private[fe] object HistoryTypography:
  // A long exercise name should ellipsize rather than making every important
  // number in the history row tiny, so metadata is not part of the shared fit.
  private val ItemSelector = ".workout-date, .workout-figures, .workout-duration"
  private val MeasuringSizePixels = 10.0
  private val BreathingRoom = 0.96
  private val MobileFillRatio = 0.8
  private val SecondaryRatio = 0.6
  private val MinimumPixels = 9.5
  private val MaximumPixels = 20.0
  private val MobileBreakpoint = 768.0

  def fit(root: dom.html.Element): Unit =
    if dom.window.innerWidth <= MobileBreakpoint then fitMobile(root)
    else fitWide(root)

  private def fitWide(root: dom.html.Element): Unit =
    rows(root).foreach(clearSizes)
    setSizes(root, MeasuringSizePixels)
    val fitted = smallestFit(root, ItemSelector)
    setSizes(root, legible(fitted))

  private def fitMobile(root: dom.html.Element): Unit =
    rows(root).foreach: row =>
      setSizes(row, MeasuringSizePixels)
      val firstFit = timestampFit(row, MeasuringSizePixels).getOrElse(MinimumPixels)
      setSizes(row, math.max(MinimumPixels, firstFit))
      // Font metrics are not perfectly linear at small sizes because hinting and letter spacing snap to device pixels.
      // Two corrections at the chosen sizes make the visible text, rather than an estimate from 10px, fill the target.
      val corrected = timestampFit(row, math.max(MinimumPixels, firstFit)).getOrElse(firstFit)
      setSizes(row, math.max(MinimumPixels, corrected))
      val exact = timestampFit(row, math.max(MinimumPixels, corrected)).getOrElse(corrected)
      setSizes(row, math.max(MinimumPixels, exact))

  private def timestampFit(row: dom.html.Element, currentPixels: Double): Option[Double] =
    val timestamp = row.querySelector(".workout-date").asInstanceOf[dom.html.Element]
    val available = timestamp.getBoundingClientRect().width
    val range = dom.document.createRange()
    range.selectNodeContents(timestamp)
    val rendered = range.getBoundingClientRect().width
    filledPixelsAt(currentPixels, available, rendered)

  private def setSizes(root: dom.html.Element, primaryPixels: Double): Unit =
    root.style.setProperty("--history-primary-size", cssPixels(primaryPixels))
    root.style.setProperty("--history-secondary-size", cssPixels(secondaryPixels(primaryPixels)))

  private def clearSizes(row: dom.html.Element): Unit =
    val _ = row.style.removeProperty("--history-primary-size")
    val _ = row.style.removeProperty("--history-secondary-size")

  private def rows(root: dom.html.Element): Seq[dom.html.Element] =
    val found = root.querySelectorAll(".workout-row")
    (0 until found.length).map(index => found.item(index).asInstanceOf[dom.html.Element])

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

  private[fe] def filledPixels(availablePixels: Double, renderedAtMeasuringSizePixels: Double): Option[Double] =
    filledPixelsAt(MeasuringSizePixels, availablePixels, renderedAtMeasuringSizePixels)

  private[fe] def filledPixelsAt(
      currentPixels: Double,
      availablePixels: Double,
      renderedAtCurrentPixels: Double
  ): Option[Double] =
    Option.when(currentPixels > 0 && availablePixels > 0 && renderedAtCurrentPixels > 0):
      currentPixels * availablePixels / renderedAtCurrentPixels * MobileFillRatio

  private[fe] def secondaryPixels(primaryPixels: Double): Double = primaryPixels * SecondaryRatio

  private[fe] def legible(value: Double): Double =
    math.max(MinimumPixels, math.min(MaximumPixels, value))

  private[fe] def cssPixels(value: Double): String = f"${math.max(1.0, value)}%.2fpx"
