package sgrv.fe.bench

import org.scalajs.dom
import scala.scalajs.js

/** Speckle: a field of grey where every level of a band appears as often as every other.
  *
  * A flat fill was never realistic. Nothing a camera is pointed at is one value across a whole region -- a wall has
  * grain, a weight plate has wear and highlights, and it is that texture the sensor's denoising, its auto-exposure and
  * its compression all react to. A test painted in two flat tones asks none of that, and a detector that passes it has
  * only been shown the easiest scene there is.
  *
  * Static rather than re-drawn each frame. A field that scintillated would inject broadband noise of its own into
  * every quadrant, which is a different experiment: here the texture belongs to the scene, and only the figure moves.
  */
private[fe] object Texture:
  /** The side of one tile, repeated across whatever it fills. Its area is a multiple of a band's width, which is what
    * lets every level appear exactly the same number of times rather than merely close to it.
    */
  val TileEdge = 256

  /** Every level of the band exactly as often as every other, in scrambled order.
    *
    * Dealt and shuffled rather than drawn at random. Sixty-five thousand independent draws would leave the histogram
    * visibly ragged -- some levels a few percent over, others under -- and the whole point of naming a band is that
    * the test covers it evenly, not on average over enough runs.
    */
  private[bench] def levels(band: Range, pixels: Int, draw: () => Double): Array[Int] =
    require(band.nonEmpty, "a band must hold at least one level")
    require(pixels > 0 && pixels % band.length == 0, s"$pixels pixels cannot be split evenly over ${band.length}")
    val each = pixels / band.length
    val values = Array.tabulate(pixels)(index => band(index / each))
    var last = pixels - 1
    while last > 0 do
      val pick = math.min(last, (draw() * (last + 1)).toInt)
      val held = values(last)
      values(last) = values(pick)
      values(pick) = held
      last -= 1
    values

  private var tiles = Map.empty[Range, dom.HTMLCanvasElement]

  /** A tile of this band, built once and kept. Two bands means two tiles for a whole suite. */
  private def tile(band: Range): dom.HTMLCanvasElement =
    tiles.getOrElse(
      band, {
        val built = build(band)
        tiles = tiles.updated(band, built)
        built
      }
    )

  private def build(band: Range): dom.HTMLCanvasElement =
    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
    canvas.width = TileEdge
    canvas.height = TileEdge
    val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val image = context.createImageData(TileEdge.toDouble, TileEdge.toDouble)
    val values = levels(band, TileEdge * TileEdge, () => scala.util.Random.nextDouble())
    var index = 0
    while index < values.length do
      val level = values(index)
      val at = index * 4
      image.data(at) = level
      image.data(at + 1) = level
      image.data(at + 2) = level
      image.data(at + 3) = 255
      index += 1
    context.putImageData(image, 0, 0)
    canvas

  /** This band as something a canvas can paint with. */
  def paint(context: dom.CanvasRenderingContext2D, band: Range): js.Any =
    context.createPattern(tile(band), "repeat").asInstanceOf[js.Any]
