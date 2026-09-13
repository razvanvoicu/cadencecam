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

  test("sensitivity is held with the exposure, not left to the driver"):
    // Switching exposure to manual stops the camera choosing its sensitivity too. A request naming only the shutter
    // leaves it wherever the driver puts it -- as low as 21 on a camera metering at 100.
    val exposure = Camera.manualControls.find(_.mode == "exposureMode").get

    assertEquals(exposure.settings, Seq("exposureTime", "iso"))
