package sgrv.fe.bench

final class RigSuite extends munit.FunSuite:

  test("every device is offered once"):
    assertEquals(Rig.Devices.distinct, Rig.Devices)

  test("the defaults are among the choices"):
    assert(Rig.Devices.contains(Rig.DefaultDevice))
    assert(Rig.Cameras.contains(Rig.DefaultCamera))

  test("both cameras are offered"):
    assertEquals(Rig.Cameras, Seq("Back camera", "Front camera"))

  /** The note is parsed when the recordings are read back, so its shape is part of the recording, not decoration. */
  test("the choice reads as a comma-separated tail"):
    assertEquals(Rig.describe("Pixel", "Back camera"), "Pixel, Back camera")

  test("a name with a space of its own survives"):
    assertEquals(Rig.describe("Yoga battery problem", "Front camera"), "Yoga battery problem, Front camera")
