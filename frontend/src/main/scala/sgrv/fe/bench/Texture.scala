package sgrv.fe.bench

import org.scalajs.dom
import scala.scalajs.js

/** A 45-degree brightness gradient across a filled region, from the bottom of its band to the top.
  *
  * The darkest level of the band sits at the region's bottom-left and the brightest at its top-right, varying linearly
  * along the diagonal between them. The band itself is unchanged: a region still spans exactly the grey levels its
  * palette names, laid out as a ramp rather than scattered as speckle.
  */
private[fe] object Texture:

  /** The ends of a 45-degree gradient line across a box, from its bottom-left to its top-right, in canvas coordinates.
    *
    * Canvas y grows downwards, so bottom-left is the larger y. The line runs along (1, -1) through the box's centre and
    * is as long as the box's extent in that direction, so its ends are the box's own corners when the box is square and
    * the direction stays at 45 degrees when it is not.
    */
  private[bench] def diagonal(
      minX: Double,
      minY: Double,
      maxX: Double,
      maxY: Double
  ): (Double, Double, Double, Double) =
    val centreX = (minX + maxX) / 2
    val centreY = (minY + maxY) / 2
    val reach = ((maxX - minX) + (maxY - minY)) / 4
    (centreX - reach, centreY + reach, centreX + reach, centreY - reach)

  /** This band as a gradient over the given box: its lowest level at the bottom-left, its highest at the top-right. */
  def paint(
      context: dom.CanvasRenderingContext2D,
      band: Range,
      minX: Double,
      minY: Double,
      maxX: Double,
      maxY: Double
  ): js.Any =
    require(band.nonEmpty, "a band must hold at least one level")
    val (x0, y0, x1, y1) = diagonal(minX, minY, maxX, maxY)
    val gradient = context.createLinearGradient(x0, y0, x1, y1)
    gradient.addColorStop(0, grey(band.head))
    gradient.addColorStop(1, grey(band.last))
    gradient.asInstanceOf[js.Any]

  private[bench] def grey(level: Int): String = s"rgb($level, $level, $level)"
