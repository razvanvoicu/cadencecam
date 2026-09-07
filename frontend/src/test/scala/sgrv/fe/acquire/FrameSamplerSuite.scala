package sgrv.fe.acquire

import munit.FunSuite

class FrameSamplerSuite extends FunSuite:

  /** Builds an RGBA buffer whose pixels are set by a function of their coordinates. */
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

  test("reads the four quadrants in top-left, top-right, bottom-left, bottom-right order"):
    // One distinct value per quadrant, so a transposed or rotated read is visible in the result.
    val pixels = frame(4, 4): (x, y) =>
      (if y < 2 then 0 else 2) + (if x < 2 then 0 else 1) match
        case 0 => 10
        case 1 => 20
        case 2 => 30
        case 3 => 40

    val quadrants = FrameSampler.quadrantLuma(pixels, 4, 4)

    assertEquals(quadrants.map(_.round.toInt), Seq(10, 20, 30, 40))

  test("a uniform frame gives every quadrant the same brightness"):
    val quadrants = FrameSampler.quadrantLuma(frame(8, 8)((_, _) => 128), 8, 8)

    assertEquals(quadrants.distinct.size, 1)
    assertEqualsDouble(quadrants.head, 128.0, 0.001)

  test("brightness is a weighted luma, not a plain channel average"):
    // Pure green is much brighter to the eye than pure blue; a naive average would rate them equally.
    def solid(r: Int, g: Int, b: Int): Array[Int] =
      Array.tabulate(2 * 2 * 4): index =>
        index % 4 match
          case 0 => r
          case 1 => g
          case 2 => b
          case _ => 255

    val green = FrameSampler.quadrantLuma(solid(0, 255, 0), 2, 2).head
    val blue = FrameSampler.quadrantLuma(solid(0, 0, 255), 2, 2).head

    assertEqualsDouble(green, 0.7152 * 255, 0.001)
    assertEqualsDouble(blue, 0.0722 * 255, 0.001)
    assert(green > blue, "green must read brighter than blue")

  test("a moving bright band changes the quadrants it crosses and leaves the others alone"):
    // The signal the rep detector consumes: a bright region sweeping top to bottom.
    val top = FrameSampler.quadrantLuma(frame(8, 8)((_, y) => if y < 4 then 255 else 0), 8, 8)
    val bottom = FrameSampler.quadrantLuma(frame(8, 8)((_, y) => if y >= 4 then 255 else 0), 8, 8)

    assertEquals(top.map(_.round.toInt), Seq(255, 255, 0, 0))
    assertEquals(bottom.map(_.round.toInt), Seq(0, 0, 255, 255))

  test("the sample canvas keeps the frame's aspect ratio and stays small and even"):
    val (landscapeWidth, landscapeHeight) = FrameSampler.sampleSize(1280, 720)
    val (squareWidth, squareHeight) = FrameSampler.sampleSize(480, 480)
    val (portraitWidth, portraitHeight) = FrameSampler.sampleSize(720, 1280)

    assertEquals(landscapeWidth, FrameSampler.SampleWidth)
    // 64 * 720/1280 = 36, rounded up to an even number so the quadrant split is exact.
    assertEquals(landscapeHeight, 36)
    assertEquals((squareWidth, squareHeight), (64, 64))
    assert(portraitHeight > portraitWidth, s"portrait must stay taller than wide, got $portraitWidth×$portraitHeight")
    Seq(landscapeHeight, squareHeight, portraitHeight).foreach: height =>
      assertEquals(height % 2, 0, s"$height must be even")
      assert(height >= 2, s"$height must be usable")

  test("samples a whole megapixel frame into a few thousand pixels"):
    val (width, height) = FrameSampler.sampleSize(Camera.PreferredWidth, Camera.PreferredHeight)

    assert(Camera.PreferredWidth * Camera.PreferredHeight <= 1_000_000, "the capture must stay under a megapixel")
    assert(width * height < 4000, s"a sample reads $width×$height pixels, which is too many for a phone at 10 Hz")

  test("samples at roughly ten hertz"):
    assertEquals(FrameSampler.DefaultIntervalMillis, 100)
