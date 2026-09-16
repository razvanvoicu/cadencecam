package sgrv.fe.acquire

import scala.scalajs.js

final class CameraAdjustSuite extends munit.FunSuite:

  private def range(min: Double, max: Double, step: Double) =
    js.Dynamic.literal("min" -> min, "max" -> max, "step" -> step)

  test("a setting reported as a range becomes a slider, seated where the camera is"):
    val capabilities = js.Dynamic.literal("exposureTime" -> range(0.26, 160000.0, 0.1))
    val settings = js.Dynamic.literal("exposureTime" -> 83.3)

    val found = CameraAdjust.adjustable(capabilities, settings)

    assertEquals(found.map(_.setting), Seq("exposureTime"))
    assertEquals(found.head.current, 83.3)
    assertEquals(found.head.min, 0.26)
    assertEquals(found.head.max, 160000.0)

  test("the exposure slider stops where the frame rate would start dropping frames"):
    // Exposure runs in hundreds of microseconds: at 30 frames a second, one frame is 333 of them. The camera offers up
    // to sixteen seconds, and everything past one frame's worth is bought with the sampling rate the detector needs.
    val capabilities = js.Dynamic.literal("exposureTime" -> range(0.26, 160000.0, 0.1))

    val found = CameraAdjust
      .adjustable(capabilities, js.Dynamic.literal("exposureTime" -> 83.3, "frameRate" -> 30))
      .head

    assertEqualsDouble(found.max, 10000.0 / 30, 0.001)
    assertEqualsDouble(CameraAdjust.ceiling("exposureTime", 160000.0, Some(60)), 10000.0 / 60, 0.001)

  test("without a frame rate the camera's own maximum stands, and other settings are never capped"):
    assertEqualsDouble(CameraAdjust.ceiling("exposureTime", 160000.0, None), 160000.0, 0.001)
    assertEqualsDouble(CameraAdjust.ceiling("iso", 5333.0, Some(30)), 5333.0, 0.001)

  test("a setting reported as modes rather than a range is not offered"):
    // Safari reports exposure this way when it reports it at all; a slider with no span cannot be moved.
    val capabilities = js.Dynamic.literal("exposureMode" -> js.Array("continuous", "manual"))

    assertEquals(CameraAdjust.adjustable(capabilities, js.Dynamic.literal()), Seq.empty)

  test("a camera reporting nothing offers nothing"):
    assertEquals(CameraAdjust.adjustable(js.Dynamic.literal(), js.Dynamic.literal()), Seq.empty)

  test("a range with no span is not offered"):
    val capabilities = js.Dynamic.literal("iso" -> range(100, 100, 1))

    assertEquals(CameraAdjust.adjustable(capabilities, js.Dynamic.literal("iso" -> 100)), Seq.empty)

  test("a capability of the wrong shape is ignored rather than becoming a NaN slider"):
    // A bound that is not a number renders a slider that cannot be moved, which is worse than no slider.
    val capabilities = js.Dynamic.literal("iso" -> js.Dynamic.literal("min" -> "low", "max" -> 3200))

    assertEquals(CameraAdjust.adjustable(capabilities, js.Dynamic.literal()), Seq.empty)

  test("a setting the camera reports no current value for still opens at the bottom of its range"):
    val capabilities = js.Dynamic.literal("iso" -> range(21, 5333, 1))

    val found = CameraAdjust.adjustable(capabilities, js.Dynamic.literal())

    assertEquals(found.map(_.current), Seq(21.0))

  test("a missing step is derived rather than assumed to be one"):
    // Exposure runs in hundreds of microseconds and colour temperature in kelvin; a shared granularity fits neither.
    val capabilities = js.Dynamic.literal("colorTemperature" -> js.Dynamic.literal("min" -> 2850, "max" -> 7000))

    val found = CameraAdjust.adjustable(capabilities, js.Dynamic.literal())

    assertEquals(found.head.step, (7000.0 - 2850.0) / 100.0)

  test("each slider knows which mode has to go manual before it can move"):
    assertEquals(CameraAdjust.governing("exposureTime"), Some("exposureMode"))
    assertEquals(CameraAdjust.governing("iso"), Some("exposureMode"))
    assertEquals(CameraAdjust.governing("colorTemperature"), Some("whiteBalanceMode"))
    assertEquals(CameraAdjust.governing("zoom"), None)

  test("exposure is offered before sensitivity, and both before colour"):
    assertEquals(CameraAdjust.offered.map(_._1), Seq("exposureTime", "iso", "colorTemperature", "focusDistance"))

  test("exposure and sensitivity travel in proportion; colour and focus in units"):
    assertEquals(CameraAdjust.offered.filter(_._3).map(_._1), Seq("exposureTime", "iso"))

  test("a proportional slider reaches the metered value near the middle of its travel, not at the very bottom"):
    // The fault this fixes: 0.26 to 160000 linearly puts 83 at three thousandths of the track, so the smallest drag a
    // finger can make lands on a multi-second exposure and the picture goes white.
    val exposure = CameraAdjust
      .adjustable(
        js.Dynamic.literal("exposureTime" -> range(0.26, 160000.0, 0.1)),
        js.Dynamic.literal("exposureTime" -> 83.3)
      )
      .head

    val linear = (83.3 - 0.26) / (160000.0 - 0.26)
    val proportional = CameraAdjust.positionOf(exposure, 83.3)

    assert(linear < 0.001, s"linear would sit at $linear")
    assert(proportional > 0.4 && proportional < 0.75, s"proportional sits at $proportional")

  test("a position and its value are inverses of one another"):
    val exposure = Adjustable("exposureTime", "Exposure", Some("exposureMode"), 0.26, 160000.0, 0.1, 83.3, true)
    val colour = Adjustable("colorTemperature", "Colour", Some("whiteBalanceMode"), 2850, 7000, 50, 4200, false)

    for control <- Seq(exposure, colour); p <- Seq(0.0, 0.25, 0.5, 0.75, 1.0) do
      val round = CameraAdjust.positionOf(control, CameraAdjust.valueAt(control, p))
      assertEqualsDouble(round, p, 0.001, s"${control.setting} at $p")

  test("the ends of the track are the ends of the range"):
    val exposure = Adjustable("exposureTime", "Exposure", Some("exposureMode"), 0.26, 160000.0, 0.1, 83.3, true)

    assertEqualsDouble(CameraAdjust.valueAt(exposure, 0.0), 0.26, 0.001)
    assertEqualsDouble(CameraAdjust.valueAt(exposure, 1.0), 160000.0, 0.001)

  test("a zero minimum does not defeat a proportional scale"):
    // Cameras do report zero, and zero has no logarithm.
    val focus = Adjustable("exposureTime", "Exposure", Some("exposureMode"), 0.0, 1000.0, 0.1, 10.0, true)

    assert(!CameraAdjust.valueAt(focus, 0.5).isNaN)
    assert(!CameraAdjust.positionOf(focus, 10.0).isNaN)

  test("sensitivity is held with the exposure, not left to the driver"):
    // Switching exposure to manual stops the camera choosing its sensitivity too. A request naming only the shutter
    // leaves it wherever the driver puts it -- as low as 21 on a camera metering at 100.
    val exposure = Camera.manualControls.find(_.mode == "exposureMode").get

    assertEquals(exposure.settings, Seq("exposureTime", "iso"))
