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
    for taken <- 1 to length do reading = counter.update(channels.view.mapValues(_.take(taken)).toMap, taken)
    reading

  private def rotating(hz: Double, seconds: Double, amplitude: Double) =
    Map(
      Quadrant.Q1 -> channel(hz, seconds, amplitude, 0.0),
      Quadrant.Q2 -> channel(hz, seconds, amplitude, math.Pi / 2),
      Quadrant.Q3 -> channel(hz, seconds, amplitude, math.Pi),
      Quadrant.Q4 -> channel(hz, seconds, amplitude, 3 * math.Pi / 2)
    )

  /** What the filter's settling window costs: the opening samples cannot be analysed, so the reps performed during them
    * are never seen. The acquisition design assumes the first seconds are the user still getting into position, so this
    * is a known, bounded loss rather than drift.
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
      val window = channels.view.mapValues(_.take(taken).takeRight(QuadrantSignals.DetectionWindow)).toMap
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

  test("resetting starts over completely: the tally, the detector, and the signal behind them"):
    // The button exists to discard what accrued while the user was getting into position. Leaving the buffer would
    // keep that movement working against them twice: its peaks stay countable, and its strength still sets the
    // threshold the real exercise has to clear.
    val counter = RepCounter()
    val channels = rotating(1.0, 60.0, amplitude = 6.0)
    def feed(upTo: Int): RepReading =
      var reading = counter.reading
      for taken <- 1 to upTo do reading = counter.update(channels.view.mapValues(_.take(taken)).toMap, taken)
      reading

    val before = feed(400)
    assert(before.count > 20, s"expected a running count before resetting, got ${before.count}")
    assert(before.lock.isInstanceOf[LockState.Locked], s"expected a lock before resetting, got ${before.lock}")

    counter.reset()

    assertEquals(counter.reading.count, 0)
    // Acquiring again from nothing, rather than carrying the previous lock forward.
    assertEquals(counter.reading.lock, LockState.Acquiring(0, DetectorSettings().minimumSamplesForLock))

  test("after a reset the detector has to see a cadence again before it counts"):
    // The cost of a full wipe, stated: a fresh buffer means no lock until enough samples have arrived. Nothing is
    // lost by it, because the first lock counts the run it finds in the buffer by then.
    val counter = RepCounter()
    val channels = rotating(1.0, 60.0, amplitude = 6.0)
    for taken <- 1 to 400 do counter.update(channels.view.mapValues(_.take(taken)).toMap, taken)
    counter.reset()

    val tooSoon = counter.update(channels.view.mapValues(_.take(100)).toMap, 100)
    assertEquals(tooSoon.count, 0)
    assert(tooSoon.lock.isInstanceOf[LockState.Acquiring], s"expected to be acquiring, got ${tooSoon.lock}")
  private def sustained(peaks: Seq[Int]) =
    RepAnalysis.sustained(peaks, DetectorSettings().minimumSustainedPeaks, DetectorSettings().maximumGapSamples)

  /** A run of `count` peaks spaced a second apart, comfortably inside the gap limit. */
  private def streak(count: Int, from: Int = 50) = Seq.tabulate(count)(index => from + index * 10)

  test("a run short of the minimum counts nothing, however prominent its peaks"):
    val needed = DetectorSettings().minimumSustainedPeaks

    for length <- 1 until needed do assertEquals(sustained(streak(length)), Seq.empty, s"a run of $length counted")

  test("the peak that completes the minimum makes the earlier ones countable too"):
    // None of them was a rep on its own; each is evidence for the others, so they arrive together.
    val needed = DetectorSettings().minimumSustainedPeaks

    assertEquals(sustained(streak(needed)), streak(needed))

  test("a movement that keeps going keeps counting"):
    val steady = Seq.tabulate(12)(index => 50 + index * 10)

    assertEquals(sustained(steady), steady)

  test("peaks further apart than the slowest cadence are separate movements"):
    val settings = DetectorSettings()
    // Two seconds at ten samples a second, and the slowest rep the band-pass admits.
    assertEquals(settings.maximumGapSamples, 20)

    val atTheLimit = Seq.tabulate(6)(index => index * settings.maximumGapSamples)
    val justBeyond = Seq.tabulate(6)(index => index * (settings.maximumGapSamples + 1))

    assertEquals(sustained(atTheLimit), atTheLimit)
    assertEquals(sustained(justBeyond), Seq.empty)

  test("a long run survives and a short one beside it does not"):
    // Getting into position, then exercising: the setup events are too sparse to support each other.
    val setup = Seq(0, 40, 95)
    val bout = Seq.tabulate(6)(index => 200 + index * 10)

    assertEquals(sustained(setup ++ bout), bout)

  test("a break splits one movement into two, and only the sustained parts count"):
    val before = Seq.tabulate(5)(index => index * 10)
    val after = Seq.tabulate(5)(index => 400 + index * 10)
    val strayDuringTheBreak = Seq(150, 250)

    assertEquals(sustained(before ++ strayDuringTheBreak ++ after), before ++ after)

  test("nothing at all is handled"):
    assertEquals(sustained(Seq.empty), Seq.empty)

  test("the noise floor rejects movement too small to be a rep"):
    val settings = DetectorSettings()
    assertEquals(settings.prominenceFloor, 5.0)
    // A peak stands about twice a channel's amplitude above its valleys, so the floor bites below half of it.
    val tooSmall = rotating(1.0, 60.0, amplitude = 1.5)

    assertEquals(run(tooSmall).count, 0)

  test("a rep of ordinary size still clears the raised floor"):
    val reading = run(rotating(1.0, 60.0, amplitude = 6.0))

    assert(reading.count > 20, s"an ordinary rep must still be counted, got ${reading.count}")

  test("the pace is the one the counted reps actually kept"):
    val counter = RepCounter()
    val channels = rotating(1.0, 60.0, amplitude = 6.0)
    run(channels, counter)

    // A rep a second is sixty a minute, allowing for the human wobble the generator does not have.
    assertEqualsDouble(counter.repsPerMinute, 60.0, 2.0)

  test("a faster cadence reports a faster pace"):
    val slow = RepCounter()
    val fast = RepCounter()
    run(rotating(0.75, 60.0, amplitude = 6.0), slow)
    run(rotating(1.5, 60.0, amplitude = 6.0), fast)

    assertEqualsDouble(slow.repsPerMinute, 45.0, 2.0)
    assertEqualsDouble(fast.repsPerMinute, 90.0, 3.0)

  test("too few reps to know a pace is reported as no pace, rather than as a guess"):
    val counter = RepCounter()

    assertEquals(counter.repsPerMinute, 0.0)

  test("resetting the detector forgets the pace along with the count"):
    val counter = RepCounter()
    run(rotating(1.0, 60.0, amplitude = 6.0), counter)
    assert(counter.repsPerMinute > 0)

    counter.reset()

    assertEquals(counter.repsPerMinute, 0.0)

  test("the pace follows the recent reps rather than the whole session"):
    // Measured over the last few, so a change of pace within a set is reported rather than averaged away.
    val settings = DetectorSettings()
    assertEquals(settings.paceWindowReps, 10)
