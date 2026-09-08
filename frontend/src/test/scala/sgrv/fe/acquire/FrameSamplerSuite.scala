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

  test("the sample canvas keeps the frame's aspect ratio, in either orientation"):
    val (landscapeWidth, landscapeHeight) = FrameSampler.sampleSize(1280, 720)
    val (portraitWidth, portraitHeight) = FrameSampler.sampleSize(720, 1280)

    assertEqualsDouble(landscapeWidth.toDouble / landscapeHeight, 1280.0 / 720, 0.05)
    assert(portraitHeight > portraitWidth, s"portrait must stay taller than wide, got $portraitWidth×$portraitHeight")
    // A rotated frame costs the same to sample as an upright one.
    assertEquals(landscapeWidth * landscapeHeight, portraitWidth * portraitHeight)

  test("the sample canvas stays even, so the quadrant split is exact"):
    Seq((1280, 720), (720, 1280), (480, 480), (4032, 3024), (641, 481)).foreach: (frameWidth, frameHeight) =>
      val (width, height) = FrameSampler.sampleSize(frameWidth, frameHeight)
      assertEquals(width % 2, 0, s"$width is odd")
      assertEquals(height % 2, 0, s"$height is odd")
      assert(width >= 2 && height >= 2, s"$width×$height is unusable")

  test("samples a megapixel frame into a few thousand pixels, whatever shape it arrives in"):
    // The camera's shape now follows the device, so the sampler must stay cheap for any of them.
    Seq((1152, 864), (1024, 768), (1280, 720), (720, 1280), (960, 960)).foreach: (frameWidth, frameHeight) =>
      val (width, height) = FrameSampler.sampleSize(frameWidth, frameHeight)
      assert(
        width * height < 4000,
        s"$frameWidth×$frameHeight samples to $width×$height, too many pixels for a phone at 10 Hz"
      )
