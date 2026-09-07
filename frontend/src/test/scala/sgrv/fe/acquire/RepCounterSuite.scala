package sgrv.fe.acquire

import munit.FunSuite

class RepCounterSuite extends FunSuite:

  private val rate = 10.0

  /** A quadrant's brightness over time: a resting level with a cadence on top, at a given phase. */
  private def channel(hz: Double, seconds: Double, amplitude: Double, phase: Double = 0.0, level: Double = 120.0) =
    Seq.tabulate((seconds * rate).toInt): index =>
      level + amplitude * math.sin(2 * math.Pi * hz * index / rate + phase)

  private def still(seconds: Double, level: Double = 120.0) = Seq.fill((seconds * rate).toInt)(level)

  /** Feeds a whole recording through one sample at a time, exactly as the live loop does. */
  private def run(channels: Map[Quadrant, Seq[Double]], counter: RepCounter = RepCounter()): RepReading =
    val length = channels.values.map(_.size).min
    var reading = counter.reading
    for taken <- 1 to length do
      reading = counter.update(channels.view.mapValues(_.take(taken)).toMap, taken)
    reading

  private def rotating(hz: Double, seconds: Double, amplitude: Double) =
    Map(
      Quadrant.Q1 -> channel(hz, seconds, amplitude, 0.0),
      Quadrant.Q2 -> channel(hz, seconds, amplitude, math.Pi / 2),
      Quadrant.Q3 -> channel(hz, seconds, amplitude, math.Pi),
      Quadrant.Q4 -> channel(hz, seconds, amplitude, 3 * math.Pi / 2)
    )

  /** What the filter's settling window costs: the opening samples cannot be analysed, so the reps performed during
    * them are never seen. The acquisition design assumes the first seconds are the user still getting into
    * position, so this is a known, bounded loss rather than drift.
    */
  private def settlingLoss(hz: Double) =
    DetectorSettings().settlingSamples / rate * hz

  test("counts one rep per cycle of a steady cadence, losing only the filter's settling window"):
    // 40 seconds at 0.556 Hz is the stair-climber example from the design notes: about 22 cycles.
    Seq((0.556, 40.0), (1.0, 40.0), (1.0, 80.0), (1.5, 60.0)).foreach: (hz, seconds) =>
      val reading = run(rotating(hz, seconds, amplitude = 6.0))
      val cycles = hz * seconds

      assert(reading.lock.isInstanceOf[LockState.Locked], s"$hz Hz: expected a lock, got ${reading.lock}")
      // Never more than really happened: a count that invents reps is worse than one that misses the opening few.
      assert(reading.count <= cycles, f"$hz%.3f Hz over ${seconds}%.0fs counted ${reading.count} of $cycles%.1f")
      val floor = cycles - settlingLoss(hz) - 2
      assert(reading.count >= floor, f"$hz%.3f Hz counted ${reading.count}, below the tolerable $floor%.1f")

  test("the error stays constant as a session runs on, rather than accumulating"):
    // Proportional error would mean each cycle is miscounted; a constant one means only the opening is missed.
    val shortRun = run(rotating(1.0, 40.0, amplitude = 6.0)).count
    val longRun = run(rotating(1.0, 80.0, amplitude = 6.0)).count

    assertEquals(40 - shortRun, 80 - longRun, "the shortfall must not grow with the length of the session")

  test("reports the cadence it locked onto"):
    val reading = run(rotating(1.0, 40.0, amplitude = 6.0))

    reading.lock match
      case LockState.Locked(_, _, periodSeconds) => assertEqualsDouble(periodSeconds, 1.0, 0.15)
      case other                                 => fail(s"expected a lock, got $other")

  test("counts nothing at all from a still scene"):
    val reading = run(Quadrant.All.map(_ -> still(40.0)).toMap)

    assertEquals(reading.count, 0)
    assertEquals(reading.lock, LockState.Searching)

  test("says it is still acquiring before it has enough signal to judge"):
    val reading = run(rotating(1.0, 5.0, amplitude = 6.0))

    assert(reading.lock.isInstanceOf[LockState.Acquiring], s"expected to still be acquiring, got ${reading.lock}")
    assertEquals(reading.count, 0)

  test("reveals the reps performed before the lock rather than losing them"):
    // The design's rule: no invented peaks, but peaks genuinely found late are still counted.
    val reading = run(rotating(1.0, 30.0, amplitude = 6.0))

    assert(reading.lock.isInstanceOf[LockState.Locked])
    // A lock is only attempted after 15s, yet most of the 30s of cycles is accounted for.
    assert(reading.count > 20, s"expected the pre-lock reps to be revealed, counted ${reading.count}")

  test("the count never goes backwards as the window slides"):
    val counter = RepCounter()
    val channels = rotating(0.8, 90.0, amplitude = 6.0)
    val length = channels.values.map(_.size).min
    var previous = 0

    // Ninety seconds is longer than the one-minute buffer, so this also covers the ring wrapping.
    for taken <- 1 to length do
      val window = channels.view.mapValues(_.take(taken).takeRight(QuadrantSignals.OneMinute)).toMap
      val count = counter.update(window, taken).count
      assert(count >= previous, s"count fell from $previous to $count at sample $taken")
      previous = count

    assert(previous > 60, s"expected roughly 72 reps over 90s at 0.8 Hz, counted $previous")

  test("holds the channel it is counting from, rather than alternating between quadrants"):
    // Quadrants share a period but differ in phase. A leader that changed between updates would interleave two
    // phases of one movement and count each cycle twice, which is how this was originally wrong.
    val counter = RepCounter()
    val channels = rotating(1.0, 60.0, amplitude = 6.0)
    val length = channels.values.map(_.size).min
    var leaders = Set.empty[Quadrant]

    for taken <- 1 to length do
      counter.update(channels.view.mapValues(_.take(taken)).toMap, taken).lock match
        case LockState.Locked(channel, _, _) => leaders += channel
        case _                               => ()

    assertEquals(leaders.size, 1, s"the authoritative channel changed during the session: $leaders")

  test("a gain step common to every quadrant is not counted as reps"):
    // Uncorrected, this rings through the band-pass; the detector is fed corrected channels for exactly this reason.
    val steps = Seq.tabulate(400)(index => if (index / 40) % 2 == 0 then 100.0 else 118.0)
    val raw = Quadrant.All.map(_ -> steps).toMap
    val reading = run(CommonMode.remove(raw))

    assertEquals(reading.count, 0)

  test("resetting clears the count and the lock"):
    val counter = RepCounter()
    run(rotating(1.0, 30.0, amplitude = 6.0), counter)
    counter.reset()

    assertEquals(counter.reading.count, 0)
    assertEquals(counter.reading.lock, LockState.Acquiring(0, DetectorSettings().minimumSamplesForLock))

  test("two channels agreeing is enough, even when the other two carry nothing"):
    // The design's condition is that two of four agree, not that most do: a movement may cross only two quadrants.
    val moving = channel(1.0, 40.0, amplitude = 6.0)
    val channels = Map(
      Quadrant.Q1 -> moving,
      Quadrant.Q2 -> channel(1.0, 40.0, amplitude = 6.0, phase = math.Pi),
      Quadrant.Q3 -> still(40.0),
      Quadrant.Q4 -> still(40.0)
    )

    val reading = run(channels)

    reading.lock match
      case LockState.Locked(leader, partner, _) =>
        assert(Set(leader, partner) == Set(Quadrant.Q1, Quadrant.Q2), s"locked onto $leader with $partner")
      case other => fail(s"expected a lock on the two moving quadrants, got $other")
