package sgrv.fe.acquire

/** The four quadrants of a frame, numbered the way mathematics numbers them: Q1 is top-right, and the rest follow
  * counter-clockwise. Screen coordinates run downwards, so Q1 and Q2 are the top half and Q3 and Q4 the bottom.
  *
  * {{{
  *   Q2 | Q1
  *   ---+---
  *   Q3 | Q4
  * }}}
  */
private[fe] enum Quadrant:
  case Q1, Q2, Q3, Q4

private[fe] object Quadrant:
  val All: Seq[Quadrant] = Seq(Q1, Q2, Q3, Q4)

  /** Half-open pixel bounds of a quadrant, as (xFrom, xUntil, yFrom, yUntil). An odd dimension gives the extra row or
    * column to the right/bottom quadrants, which only matters at sizes far smaller than anything sampled here.
    */
  private[acquire] def bounds(quadrant: Quadrant, width: Int, height: Int): (Int, Int, Int, Int) =
    val midX = width / 2
    val midY = height / 2
    quadrant match
      case Q1 => (midX, width, 0, midY)
      case Q2 => (0, midX, 0, midY)
      case Q3 => (0, midX, midY, height)
      case Q4 => (midX, width, midY, height)

  /** Mean Rec.709 luma of one quadrant of an RGBA buffer, on the same 0-255 scale as the channels themselves.
    *
    * This is the measure the whole detector rests on: one number per quadrant per sample, whose movement over time
    * carries the periodicity. Weighted luma rather than a channel average, so a green object moving against a blue
    * background registers the way the eye would see it rather than washing out.
    *
    * Kept free of the DOM so it can be tested directly on synthetic frames.
    */
  private[acquire] def brightness(rgba: Array[Int], width: Int, height: Int, quadrant: Quadrant): Double =
    require(width > 0 && height > 0, "a frame must have a positive size")
    val (xFrom, xUntil, yFrom, yUntil) = bounds(quadrant, width, height)
    var total = 0.0
    var counted = 0
    var y = yFrom
    while y < yUntil do
      var x = xFrom
      while x < xUntil do
        val offset = (y * width + x) * 4
        if offset + 2 < rgba.length then
          total += 0.2126 * rgba(offset) + 0.7152 * rgba(offset + 1) + 0.0722 * rgba(offset + 2)
          counted += 1
        x += 1
      y += 1
    if counted == 0 then 0.0 else total / counted
