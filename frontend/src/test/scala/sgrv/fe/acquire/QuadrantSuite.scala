package sgrv.fe.acquire

import munit.FunSuite

class QuadrantSuite extends FunSuite:

  private def frame(width: Int, height: Int)(grey: (Int, Int) => Int): Array[Int] =
    val pixels = Array.fill(width * height * 4)(255)
    for
      y <- 0 until height
      x <- 0 until width
    do
      val offset = (y * width + x) * 4
      val value = grey(x, y)
      pixels(offset) = value
      pixels(offset + 1) = value
      pixels(offset + 2) = value
    pixels

  test("quadrants are numbered as mathematics numbers them: Q1 top-right, then counter-clockwise"):
    // Distinct value per corner, so any rotation or transposition of the mapping shows up.
    val pixels = frame(4, 4): (x, y) =>
      (x >= 2, y >= 2) match
        case (true, false)  => 10 // top-right
        case (false, false) => 20 // top-left
        case (false, true)  => 30 // bottom-left
        case (true, true)   => 40 // bottom-right

    assertEquals(Quadrant.brightness(pixels, 4, 4, Quadrant.Q1).round.toInt, 10)
    assertEquals(Quadrant.brightness(pixels, 4, 4, Quadrant.Q2).round.toInt, 20)
    assertEquals(Quadrant.brightness(pixels, 4, 4, Quadrant.Q3).round.toInt, 30)
    assertEquals(Quadrant.brightness(pixels, 4, 4, Quadrant.Q4).round.toInt, 40)

  test("the four quadrants tile the frame without overlapping"):
    val seen = for
      quadrant <- Quadrant.All
      (xFrom, xUntil, yFrom, yUntil) = Quadrant.bounds(quadrant, 8, 6)
      x <- xFrom until xUntil
      y <- yFrom until yUntil
    yield (x, y)

    assertEquals(seen.size, 8 * 6, "every pixel must belong to exactly one quadrant")
    assertEquals(seen.distinct.size, seen.size, "no pixel may belong to two quadrants")

  test("brightness is weighted luma, so green reads brighter than blue"):
    def solid(r: Int, g: Int, b: Int) =
      Array.tabulate(2 * 2 * 4)(index => index % 4 match { case 0 => r; case 1 => g; case 2 => b; case _ => 255 })

    val green = Quadrant.brightness(solid(0, 255, 0), 2, 2, Quadrant.Q1)
    val blue = Quadrant.brightness(solid(0, 0, 255), 2, 2, Quadrant.Q1)

    assertEqualsDouble(green, 0.7152 * 255, 0.001)
    assertEqualsDouble(blue, 0.0722 * 255, 0.001)

  test("a hand crossing the top half moves Q1 and Q2 and leaves Q3 and Q4 alone"):
    val still = frame(8, 8)((_, _) => 100)
    val covered = frame(8, 8)((_, y) => if y < 4 then 40 else 100)

    def readings(pixels: Array[Int]) = Quadrant.All.map(Quadrant.brightness(pixels, 8, 8, _))

    val before = readings(still)
    val after = readings(covered)

    assert(after(0) < before(0), "Q1 must darken")
    assert(after(1) < before(1), "Q2 must darken")
    assertEqualsDouble(after(2), before(2), 0.001)
    assertEqualsDouble(after(3), before(3), 0.001)
