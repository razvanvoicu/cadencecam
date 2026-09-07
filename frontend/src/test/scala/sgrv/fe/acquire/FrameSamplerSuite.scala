package sgrv.fe.acquire

import munit.FunSuite

class FrameSamplerSuite extends FunSuite:

  test("samples at roughly ten hertz"):
    assertEquals(FrameSampler.DefaultIntervalMillis, 100)

  test("a frame yields one brightness per quadrant"):
    val pixels = Array.tabulate(4 * 4 * 4): index =>
      if index % 4 == 3 then 255 else 128

    val brightnesses = FrameSampler.brightnesses(pixels, 4, 4)

    assertEquals(brightnesses.keySet, Quadrant.All.toSet)
    brightnesses.values.foreach(value => assertEqualsDouble(value, 128.0, 0.001))

  test("the sample canvas keeps the frame's aspect ratio and stays small and even"):
    val (landscapeWidth, landscapeHeight) = FrameSampler.sampleSize(1280, 720)
    val (squareWidth, squareHeight) = FrameSampler.sampleSize(480, 480)
    val (portraitWidth, portraitHeight) = FrameSampler.sampleSize(720, 1280)

    assertEquals(landscapeWidth, FrameSampler.SampleWidth)
    // 64 * 720/1280 = 36, kept even so the quadrant split is exact.
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
