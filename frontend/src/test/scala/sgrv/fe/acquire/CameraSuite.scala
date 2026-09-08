package sgrv.fe.acquire

import munit.FunSuite

class CameraSuite extends FunSuite:

  private def aspect(width: Int, height: Int): Double = width.toDouble / height

  test("keeps the device's own proportions, so no field of view is cropped away"):
    // A 4:3 phone sensor must not be squeezed into 16:9; that crop is what loses the view.
    Seq((4032, 3024), (3264, 2448), (1920, 1080), (2448, 3264)).foreach: (deviceWidth, deviceHeight) =>
      val (width, height) = Camera.budgetedSize(deviceWidth, deviceHeight, Camera.PixelBudget)

      assertEqualsDouble(
        aspect(width, height),
        aspect(deviceWidth, deviceHeight),
        0.02,
        s"$deviceWidth×$deviceHeight became $width×$height, changing its shape"
      )

  test("stays within the pixel budget"):
    Seq((4032, 3024), (3264, 2448), (1920, 1080), (8000, 6000)).foreach: (deviceWidth, deviceHeight) =>
      val (width, height) = Camera.budgetedSize(deviceWidth, deviceHeight, Camera.PixelBudget)

      assert(
        width * height <= Camera.PixelBudget,
        s"$deviceWidth×$deviceHeight became $width×$height = ${width * height} pixels"
      )

  test("uses what a modest camera offers rather than scaling it up"):
    val (width, height) = Camera.budgetedSize(640, 480, Camera.PixelBudget)

    assertEquals((width, height), (640, 480))

  test("a portrait sensor stays portrait"):
    val (width, height) = Camera.budgetedSize(3024, 4032, Camera.PixelBudget)

    assert(height > width, s"portrait became $width×$height")

  test("dimensions stay even, so the quadrant split is exact"):
    Seq((4032, 3024), (1999, 1001), (641, 481)).foreach: (deviceWidth, deviceHeight) =>
      val (width, height) = Camera.budgetedSize(deviceWidth, deviceHeight, Camera.PixelBudget)

      assertEquals(width % 2, 0, s"$width is odd")
      assertEquals(height % 2, 0, s"$height is odd")

  test("a 4:3 sensor yields more field of view than the 16:9 request it replaces"):
    // The concrete gain: same budget, but the frame is no longer cut down to a widescreen strip.
    val (width, height) = Camera.budgetedSize(4032, 3024, Camera.PixelBudget)

    assert(aspect(width, height) < 1.4, f"expected a 4:3 frame, got ${aspect(width, height)}%.2f")
    assert(width * height > 1280 * 720, s"$width×$height should carry more than the old fixed 1280×720")

  test("refuses a camera reporting no size at all"):
    intercept[IllegalArgumentException](Camera.budgetedSize(0, 480, Camera.PixelBudget))
