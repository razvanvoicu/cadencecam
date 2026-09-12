package sgrv.fe.acquire

import munit.FunSuite
import scala.scalajs.js

class CameraSuite extends FunSuite:

  private def aspect(width: Int, height: Int): Double = width.toDouble / height

  test("keeps the camera's own proportions, so no field of view is cropped away"):
    // A 4:3 phone sensor must not be squeezed into 16:9; that crop is what loses the view. Turning it on end is not
    // a crop, so the shape is compared the long way round.
    Seq((4032, 3024), (3264, 2448), (1920, 1080), (2448, 3264)).foreach: (nativeWidth, nativeHeight) =>
      for portrait <- Seq(true, false) do
        val (width, height) = Camera.wantedSize(nativeWidth, nativeHeight, portrait, Camera.MinimumPixels)
        val longWays = aspect(math.max(width, height), math.min(width, height))
        val nativeLongWays = aspect(math.max(nativeWidth, nativeHeight), math.min(nativeWidth, nativeHeight))

        assertEqualsDouble(
          longWays,
          nativeLongWays,
          0.02,
          s"${nativeWidth}x$nativeHeight became ${width}x$height, changing its shape"
        )

  test("a phone held upright is asked for an upright frame"):
    // The point of the whole exercise. A sensor's long axis lies along the phone's, so the whole of what it sees
    // while the phone stands up is a tall picture; a wide one in that position is the middle band of it, with the
    // rest dropped -- and the rest is where the exercise is.
    val (width, height) = Camera.wantedSize(4032, 3024, portrait = true, Camera.MinimumPixels)

    assert(height > width, s"a phone standing upright was asked for ${width}x$height")
    assertEqualsDouble(aspect(width, height), 3.0 / 4, 0.02)

  test("a device lying down is asked for a frame lying down"):
    val (width, height) = Camera.wantedSize(4032, 3024, portrait = false, Camera.MinimumPixels)

    assert(width > height, s"a screen lying down was asked for ${width}x$height")
    assertEqualsDouble(aspect(width, height), 4.0 / 3, 0.02)

  test("the orientation comes from the screen's own shape"):
    assert(Camera.wantsPortrait(390, 780), "a phone held upright")
    assert(!Camera.wantsPortrait(780, 390), "the same phone on its side")
    assert(Camera.wantsPortrait(800, 800), "a square screen is no worse served upright")

  test("the frame carries at least the minimum, and not a great deal more"):
    // A floor rather than a budget: pixels past it buy nothing a sample can use, and cost throughput on a phone.
    Seq((4032, 3024), (3264, 2448), (1920, 1080), (8000, 6000)).foreach: (nativeWidth, nativeHeight) =>
      val (width, height) = Camera.wantedSize(nativeWidth, nativeHeight, portrait = true, Camera.MinimumPixels)

      assert(
        width * height >= Camera.MinimumPixels,
        s"${nativeWidth}x$nativeHeight became ${width}x$height = ${width * height}, under the floor"
      )
      assert(
        width * height < Camera.MinimumPixels * 1.02,
        s"${nativeWidth}x$nativeHeight became ${width}x$height, far past the floor"
      )

  test("a camera that cannot reach the floor is asked for everything it has, not more"):
    // Asking a 640x480 webcam for a megapixel invites it to answer with some other mode entirely.
    val (width, height) = Camera.wantedSize(640, 480, portrait = false, Camera.MinimumPixels)

    assertEquals((width, height), (640, 480))

  test("dimensions stay even, so the quadrant split is exact"):
    Seq((4032, 3024), (1999, 1001), (641, 481), (3024, 4032)).foreach: (nativeWidth, nativeHeight) =>
      val (width, height) = Camera.wantedSize(nativeWidth, nativeHeight, portrait = true, Camera.MinimumPixels)

      assertEquals(width % 2, 0, s"$width is odd")
      assertEquals(height % 2, 0, s"$height is odd")

  test("a 4:3 sensor yields more field of view than the 16:9 request it replaces"):
    // The concrete gain: the frame is no longer cut down to a widescreen strip.
    val (width, height) = Camera.wantedSize(4032, 3024, portrait = false, Camera.MinimumPixels)

    assert(aspect(width, height) < 1.4, f"expected a 4:3 frame, got ${aspect(width, height)}%.2f")
    assert(width * height > 1280 * 720, s"${width}x$height should carry more than the old fixed 1280x720")

  test("refuses a camera reporting no size at all"):
    intercept[IllegalArgumentException](Camera.wantedSize(0, 480, portrait = true, Camera.MinimumPixels))

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

  test("the camera is opened asking for its whole field of view"):
    // Leaving the size unsaid is what kept the view cropped: the browser then picks a default, and on Android that
    // default is a widescreen mode -- a crop chosen before the app has any say.
    val video = Camera.openingConstraints(None).asInstanceOf[js.Dynamic].video

    assertEquals(video.width.ideal, Camera.FullFieldProbe.asInstanceOf[js.Any])
    assertEquals(video.height.ideal, Camera.FullFieldProbe.asInstanceOf[js.Any])
    assertEquals(video.facingMode, "environment".asInstanceOf[js.Any])

  test("the request is square, which is what selects the widest mode a camera has"):
    // Not an arbitrary large number in each dimension. A camera's wide modes are its tall mode with the top and
    // bottom cut off, so the mode holding the most picture is the largest one nearest square -- and a browser picking
    // by fitness distance, which counts the shortfall in each dimension separately, lands on exactly that one when
    // both ideals are the same. Naming only a width would leave a 4:3 mode and its 16:9 crop equally preferred.
    val video = Camera.openingConstraints(None).asInstanceOf[js.Dynamic].video

    assertEquals(video.width.ideal, video.height.ideal)
    assert(
      Camera.FullFieldProbe > 4032,
      "the probe must exceed any sensor, or it selects a mode rather than the widest"
    )

  test("choosing a camera still asks for that camera's whole field of view"):
    val video = Camera.openingConstraints(Some("back-camera")).asInstanceOf[js.Dynamic].video

    assertEquals(video.deviceId.exact, "back-camera".asInstanceOf[js.Any])
    assertEquals(video.width.ideal, Camera.FullFieldProbe.asInstanceOf[js.Any])

  test("a frame of the same proportions is the same picture, at any size"):
    assert(Camera.sameShape((4032, 3024), (1152, 864)), "a 4:3 frame scaled down is still 4:3")
    assert(Camera.sameShape((3024, 4032), (864, 1152)), "and so is a portrait one")
    // Rounding to even dimensions moves the ratio a little, and must not read as a crop.
    assert(Camera.sameShape((1233, 999), (1232, 998)))

  test("a frame of different proportions is a crop, and is caught"):
    // The case this exists for: asking for 1152x864 and being handed 1280x720, which is a quarter of the picture
    // gone. It is the nearest mode by the distance a browser measures, so it has to be rejected afterwards.
    assert(!Camera.sameShape((1152, 864), (1280, 720)), "4:3 to 16:9 loses the top and bottom")
    assert(!Camera.sameShape((4032, 3024), (4032, 2268)))
    assert(!Camera.sameShape((864, 1152), (1152, 864)), "turning a frame on its side is not the same picture")

  test("the shape tolerance is far below the gap it has to detect"):
    val fourThree = 4.0 / 3
    val sixteenNine = 16.0 / 9

    assert(Camera.ShapeTolerance < (sixteenNine - fourThree) / sixteenNine / 4)

  test("the same picture turned on end has lost nothing, and is accepted"):
    // Turning is not cropping. A sensor whose widest mode is 4032x3024 shows exactly as much at 3024x4032, and
    // rejecting that would reject the very frame this app wants on a phone standing upright.
    assert(Camera.keptFieldOfView((4032, 3024), (3024, 4032)), "the same picture stood on end")
    assert(Camera.keptFieldOfView((4032, 3024), (866, 1156)), "stood on end and scaled down to the floor")
    assert(Camera.keptFieldOfView((4032, 3024), (1156, 866)), "left lying down and scaled down")

  test("a picture of another shape has been trimmed, and is rejected"):
    assert(!Camera.keptFieldOfView((4032, 3024), (1280, 720)), "a widescreen crop of a 4:3 sensor")
    assert(!Camera.keptFieldOfView((4032, 3024), (720, 1280)), "and the same crop stood on end")
    assert(!Camera.keptFieldOfView((4032, 3024), (1000, 1000)), "a square crop is still a crop")
