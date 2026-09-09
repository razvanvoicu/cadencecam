package sgrv.fe.acquire

import munit.FunSuite
import scala.scalajs.js

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

  test("a camera on the same side as the screen is mirrored, one facing away is not"):
    assert(Camera.mirrors(Some("user")), "a front camera shows the viewer to themselves")
    assert(!Camera.mirrors(Some("environment")), "a rear camera shows the world, which must not be reversed")

  test("a camera that does not say which way it faces is treated as facing the user"):
    // Overwhelmingly a laptop's built-in webcam, which points at whoever is using it. A rear phone camera always
    // identifies itself, so this default cannot mirror the acquisition case by mistake.
    assert(Camera.mirrors(None))

  test("there is nowhere to switch when the device has one camera or none"):
    val single = Seq(CameraDevice("a", "Front"))

    assertEquals(Camera.nextDevice(Seq.empty, None), None)
    assertEquals(Camera.nextDevice(single, Some("a")), None)
    assertEquals(Camera.nextDevice(single, None), None)

  test("switching cycles through the cameras and wraps around"):
    val all = Seq(CameraDevice("a", "Front"), CameraDevice("b", "Back"), CameraDevice("c", "Wide"))

    assertEquals(Camera.nextDevice(all, Some("a")).map(_.deviceId), Some("b"))
    assertEquals(Camera.nextDevice(all, Some("c")).map(_.deviceId), Some("a"))

  test("an unrecognised current camera starts the cycle from the beginning"):
    val all = Seq(CameraDevice("a", "Front"), CameraDevice("b", "Back"))

    assertEquals(Camera.nextDevice(all, None).map(_.deviceId), Some("a"))
    assertEquals(Camera.nextDevice(all, Some("gone")).map(_.deviceId), Some("a"))

  test("a camera with no label is named by its position"):
    assertEquals(CameraDevice.nameOf(CameraDevice("a", "Back camera"), 0), "Back camera")
    assertEquals(CameraDevice.nameOf(CameraDevice("a", ""), 1), "Camera 2")
    assertEquals(CameraDevice.nameOf(CameraDevice("a", "   "), 0), "Camera 1")

  test("only the controls a camera says it can hold manually are asked for"):
    val capabilities = js.Dynamic.literal(
      exposureMode = js.Array("none", "manual", "continuous"),
      whiteBalanceMode = js.Array("continuous"),
      focusMode = js.Array("manual", "single-shot")
    )

    assertEquals(Camera.manualCapable(capabilities), Seq("exposureMode", "focusMode"))

  test("a camera that reports no control modes is asked for nothing"):
    assertEquals(Camera.manualCapable(js.Dynamic.literal()), Seq.empty)

  test("a camera reporting the controls but not manual is asked for nothing"):
    val automaticOnly = js.Dynamic.literal(
      exposureMode = js.Array("continuous"),
      whiteBalanceMode = js.Array("continuous"),
      focusMode = js.Array("continuous")
    )

    assertEquals(Camera.manualCapable(automaticOnly), Seq.empty)

  test("a capability reported as something other than a list of modes is ignored, not trusted"):
    // Browsers vary in what they report here, and a malformed entry must not take the camera down with it.
    val odd = js.Dynamic.literal(exposureMode = "manual", whiteBalanceMode = 3, focusMode = js.Array("manual"))

    assertEquals(Camera.manualCapable(odd), Seq("focusMode"))

  test("the controls held still are the ones that re-meter on the movement being counted"):
    assertEquals(Camera.manualControls, Seq("exposureMode", "whiteBalanceMode", "focusMode"))
    // Long enough for metering to converge, short enough not to spend a set on automatic.
    assert(Camera.settleBeforeLockMillis >= 500 && Camera.settleBeforeLockMillis <= 3000)
