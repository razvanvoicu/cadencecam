package sgrv.fe.acquire

import munit.FunSuite

class CommonModeSuite extends FunSuite:

  private def channels(q1: Seq[Double], q2: Seq[Double], q3: Seq[Double], q4: Seq[Double]) =
    Map(Quadrant.Q1 -> q1, Quadrant.Q2 -> q2, Quadrant.Q3 -> q3, Quadrant.Q4 -> q4)

  private def amplitude(samples: Seq[Double]): Double =
    if samples.isEmpty then 0.0 else samples.max - samples.min

  test("a gain step common to every quadrant is removed completely"):
    // What auto-exposure looks like: every channel steps by the same amount at the same instant.
    val flat = Seq(100.0, 100.0, 100.0, 100.0)
    val stepped = flat ++ Seq(112.0, 112.0, 112.0, 112.0)
    val corrected = CommonMode.remove(channels(stepped, stepped, stepped, stepped))

    Quadrant.All.foreach: quadrant =>
      assertEqualsDouble(amplitude(corrected(quadrant)), 0.0, 1e-9)

  test("a step riding on top of movement is removed while the movement survives"):
    val wave = Seq(0.0, 4.0, 0.0, -4.0, 0.0, 4.0, 0.0, -4.0)
    val drift = Seq(0.0, 0.0, 0.0, 0.0, 20.0, 20.0, 20.0, 20.0)
    // Only Q1 is moving; every channel takes the same exposure step midway.
    val movingWithStep = wave.zip(drift).map(_ + _)
    val stillWithStep = drift

    val corrected = CommonMode.remove(channels(movingWithStep, stillWithStep, stillWithStep, stillWithStep))

    // The 20-unit step is gone from every channel; the 8-unit swing is still there, scaled by 3/4.
    assertEqualsDouble(amplitude(corrected(Quadrant.Q1)), 8.0 * 0.75, 1e-9)
    assert(amplitude(corrected(Quadrant.Q2)) < 8.0, "a still quadrant must not acquire the step")

  test("a rotation survives, because its quadrants peak at different times"):
    // Four channels of equal amplitude at 90 degrees to each other: the case that motivated keeping this simple.
    val samples = 40
    def rotating(phase: Double) =
      Seq.tabulate(samples)(index => 120.0 + 6.0 * math.sin(2 * math.Pi * index / 20.0 + phase))

    val before = channels(rotating(0), rotating(math.Pi / 2), rotating(math.Pi), rotating(3 * math.Pi / 2))
    val after = CommonMode.remove(before)

    Quadrant.All.foreach: quadrant =>
      // Their instantaneous mean is constant, so essentially none of the signal is subtracted.
      assertEqualsDouble(amplitude(after(quadrant)), amplitude(before(quadrant)), 1e-6)

  test("a vertical sweep survives, because the halves move in antiphase"):
    val samples = 40
    def sweep(sign: Double) =
      Seq.tabulate(samples)(index => 120.0 + sign * 6.0 * math.sin(2 * math.Pi * index / 20.0))

    val before = channels(sweep(1), sweep(1), sweep(-1), sweep(-1))
    val after = CommonMode.remove(before)

    Quadrant.All.foreach: quadrant =>
      assertEqualsDouble(amplitude(after(quadrant)), amplitude(before(quadrant)), 1e-6)

  test("a movement filling every quadrant in phase is cancelled, the known limitation"):
    val together = Seq.tabulate(20)(index => 120.0 + 6.0 * math.sin(2 * math.Pi * index / 10.0))
    val corrected = CommonMode.remove(channels(together, together, together, together))

    assertEqualsDouble(amplitude(corrected(Quadrant.Q1)), 0.0, 1e-9)

  test("channels of unequal length are aligned on their newest samples"):
    // A window read while a sample is being recorded must not subtract values from different instants.
    val corrected = CommonMode.remove(
      channels(Seq(1.0, 2.0, 3.0), Seq(9.0, 2.0, 3.0), Seq(2.0, 3.0), Seq(2.0, 3.0))
    )

    Quadrant.All.foreach(quadrant => assertEquals(corrected(quadrant).size, 2))
    // Aligned on the newest pair, every channel reads 2 then 3, so all deviations are zero.
    assertEqualsDouble(corrected(Quadrant.Q1).map(math.abs).max, 0.0, 1e-9)

  test("an empty set of channels is left alone"):
    assertEquals(CommonMode.remove(Map.empty), Map.empty[Quadrant, Seq[Double]])
