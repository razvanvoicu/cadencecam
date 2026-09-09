package sgrv.fe.acquire

import munit.FunSuite

class RestPhaseSuite extends FunSuite:

  private val rate = 10.0
  private val level = 120.0

  /** One quadrant's view of a set: the hand parked, then a cadence, then parked again. `restHigh` is whether the hand
    * rests inside this quadrant, which is what decides where its resting level sits in its own range.
    */
  private def channel(stillSamples: Int, cycles: Int, period: Double, amplitude: Double, restHigh: Boolean) =
    val rest = if restHigh then level + amplitude else level - amplitude
    val moving = Seq.tabulate((cycles * period).toInt): index =>
      val phase = 2 * math.Pi * index / period
      if restHigh then level + amplitude * math.cos(phase) else level - amplitude * math.cos(phase)
    Seq.fill(stillSamples)(rest) ++ moving

  private def curl(stillSamples: Int = 40) =
    Map(
      Quadrant.Q1 -> channel(stillSamples, 20, 10.0, 5.0, restHigh = false),
      Quadrant.Q2 -> channel(stillSamples, 20, 10.0, 6.0, restHigh = false),
      Quadrant.Q3 -> Seq.fill(stillSamples + 200)(level),
      Quadrant.Q4 -> channel(stillSamples, 20, 10.0, 8.0, restHigh = true)
    )

  test("the opening stillness is measured across all channels together"):
    // q3 is the stationary elbow: motionless from beginning to end. Asked on its own it would report the whole
    // recording as still, which is why the hand being at rest has to be one fact about the scene rather than four.
    //
    // A sample either way: a movement that starts from rest starts at the resting level, so the first sample of it
    // is indistinguishable from stillness. That is the truth about the recording rather than an error in reading it.
    assertEqualsDouble(RestPhase.stillPrefix(curl(stillSamples = 40)).toDouble, 40.0, 1.0)
    assertEqualsDouble(RestPhase.stillPrefix(curl(stillSamples = 5)).toDouble, 5.0, 1.0)

  test("a recording that opens mid-movement reports no stillness"):
    val moving = Map(
      Quadrant.Q1 -> channel(0, 20, 10.0, 5.0, restHigh = false),
      Quadrant.Q4 -> channel(0, 20, 10.0, 8.0, restHigh = true)
    )

    assert(RestPhase.stillPrefix(moving) <= 1, s"found ${RestPhase.stillPrefix(moving)} still samples in a movement")

  test("camera noise on a still scene does not read as movement"):
    val noisy = Map(
      Quadrant.Q1 -> Seq.tabulate(60)(index => level + (if index % 2 == 0 then 0.4 else -0.4)),
      Quadrant.Q4 -> Seq.tabulate(60)(index => level + (if index % 3 == 0 then 0.5 else -0.3))
    )

    assertEquals(RestPhase.stillPrefix(noisy), 60)

  test("the hand rests at the top of its own quadrant's range and the bottom of the others'"):
    val recording = curl()
    val still = RestPhase.stillPrefix(recording)

    assertEquals(RestPhase.restFraction(recording(Quadrant.Q4), still).map(_.round), Some(1L))
    assertEquals(RestPhase.restFraction(recording(Quadrant.Q2), still).map(_.round), Some(0L))

  test("a channel that says nothing is reported as saying nothing, rather than guessed at"):
    val recording = curl()
    val still = RestPhase.stillPrefix(recording)

    // The stationary elbow never sweeps a range, so where rest sits within it is undefined.
    assertEquals(RestPhase.restFraction(recording(Quadrant.Q3), still), None)
    // Too little opening stillness to establish a resting level at all.
    assertEquals(RestPhase.restFraction(recording(Quadrant.Q4), stillFor = 3), None)

  test("resting where a channel is brightest puts its peak on the rep boundary"):
    assertEquals(RestPhase.peakOffset(1.0), 0.0)

  test("resting where a channel is darkest puts its peak half a rep away"):
    assertEquals(RestPhase.peakOffset(0.0), 0.5)

  test("a channel crossed midway through the travel falls between the two"):
    assertEqualsDouble(RestPhase.peakOffset(0.5), 0.25, 1e-9)

  test("the offset is measured for whichever channel is asked about"):
    val recording = curl()

    // Counting from q4, the hand's resting place, ticks on the boundary the exerciser counts.
    assertEqualsDouble(RestPhase.offsetOf(recording, Quadrant.Q4).get, 0.0, 0.02)
    // Counting from q2, the far end of the arc, ticks half a rep out -- the same reps, the same rate, a different
    // moment. Which of the two leads is decided by lighting, so this is the number that explains the difference.
    assertEqualsDouble(RestPhase.offsetOf(recording, Quadrant.Q2).get, 0.5, 0.02)

  test("no opening stillness means no anchor, and no offset is claimed"):
    val recording = curl(stillSamples = 2)

    assertEquals(RestPhase.offsetOf(recording, Quadrant.Q4), None)
