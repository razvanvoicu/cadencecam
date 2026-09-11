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

    assertEquals(Camera.manualCapable(capabilities).map(_.mode), Seq("exposureMode", "focusMode"))

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

    assertEquals(Camera.manualCapable(odd).map(_.mode), Seq("focusMode"))

  test("the controls held still are the ones that re-meter on the movement being counted"):
    assertEquals(Camera.manualControls.map(_.mode), Seq("exposureMode", "whiteBalanceMode", "focusMode"))
    // Long enough for metering to converge, short enough not to spend a set on automatic.
    assert(Camera.settleBeforeLockMillis >= 500 && Camera.settleBeforeLockMillis <= 3000)

  test("holding a control still carries the value it settled on, not only the mode"):
    val settled = js.Dynamic.literal("exposureTime" -> 312.5, "colorTemperature" -> 4200)

    val exposure = Camera.pinning(ManualControl("exposureMode", "exposureTime"), settled).get

    assertEquals(exposure.exposureMode.asInstanceOf[String], "manual")
    assertEquals(exposure.exposureTime.asInstanceOf[Double], 312.5)

  test("a control the camera reports no value for is left automatic rather than pinned to nothing"):
    // Cameras advertise a manual mode while reporting no current value for it, and asking for the mode alone tells
    // the camera to stop deciding without saying what to do instead. What it then picks is nothing in particular:
    // this is the case that turned a correct exposure dark a second after opening.
    val settled = js.Dynamic.literal("exposureTime" -> 312.5)

    assertEquals(Camera.pinning(ManualControl("focusMode", "focusDistance"), settled), None)
    assertEquals(Camera.pinning(ManualControl("whiteBalanceMode", "colorTemperature"), settled), None)

  test("a camera reporting no settings at all is left entirely alone"):
    val nothing = js.Dynamic.literal()

    assert(Camera.manualControls.forall(control => Camera.pinning(control, nothing).isEmpty))

  test("each control is pinned by its own setting"):
    assertEquals(
      Camera.manualControls.map(control => control.mode -> control.setting).toMap,
      Map(
        "exposureMode" -> "exposureTime",
        "whiteBalanceMode" -> "colorTemperature",
        "focusMode" -> "focusDistance"
      )
    )

  test("holding the controls still is switched off, pending evidence that it helps"):
    // It was suspected of darkening a phone's picture and cleared by measurement: that device exposed neither
    // getCapabilities nor getSettings, so none of this could run, and the darkening happened anyway. Off until a
    // trace shows it was ever involved.
    assert(!Camera.holdControls)

  test("the camera's own proportions are kept exactly, portrait or landscape"):
    // A phone standing portrait delivers a portrait frame, and that is the whole field of view its sensor offers in
    // that orientation. Asking for landscape can only be satisfied by throwing some of it away.
    val (portraitW, portraitH) = Camera.bestSize(3024, 4032, 4032, 4032, Camera.PixelBudget)
    val (landscapeW, landscapeH) = Camera.bestSize(4032, 3024, 4032, 4032, Camera.PixelBudget)

    assertEqualsDouble(portraitW.toDouble / portraitH, 3024.0 / 4032.0, 0.01)
    assertEqualsDouble(landscapeW.toDouble / landscapeH, 4032.0 / 3024.0, 0.01)
    assert(portraitH > portraitW, s"a portrait frame must stay portrait, got ${portraitW}x$portraitH")
    assert(landscapeW > landscapeH, s"a landscape frame must stay landscape, got ${landscapeW}x$landscapeH")

  test("the frame is the largest that fits the budget at the camera's own shape"):
    val (width, height) = Camera.bestSize(4032, 3024, 4032, 4032, Camera.PixelBudget)

    assert(width * height <= Camera.PixelBudget, s"${width}x$height exceeds the budget")
    // And not wastefully small: within a few percent of the budget.
    assert(width * height > Camera.PixelBudget * 0.9, s"${width}x$height wastes most of the budget")

  test("the camera's own limits bound the request without reshaping it"):
    // The two maxima are separate numbers and need not belong to one supported mode, so they are used as bounds.
    val (width, height) = Camera.bestSize(3024, 4032, 720, 1280, Camera.PixelBudget)

    assert(width <= 720 && height <= 1280, s"${width}x$height exceeds what the camera reported")
    assertEqualsDouble(width.toDouble / height, 3024.0 / 4032.0, 0.01)

  test("dimensions stay even, so the quadrant split is exact"):
    val sizes = Seq((3024, 4032), (4032, 3024), (1080, 1920), (640, 480), (1233, 999))
    sizes.foreach: (w, h) =>
      val (width, height) = Camera.bestSize(w, h, 8000, 8000, Camera.PixelBudget)
      assertEquals(width % 2, 0, s"width $width from ${w}x$h is odd")
      assertEquals(height % 2, 0, s"height $height from ${w}x$h is odd")
