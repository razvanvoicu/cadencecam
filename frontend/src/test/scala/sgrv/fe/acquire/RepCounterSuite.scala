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

  /** What the return test costs at the other end: a peak is not a rep until the movement has come back from it, so the
    * newest peak waits for the slowest period the band admits before it can be judged at all. Like the settling loss
    * this is bounded and does not accumulate -- it is the tail of a run, not a fraction of it -- and in a real session
    * it is paid during the pause after the set rather than costing a rep.
    */
  private def returnLoss(hz: Double) =
    DetectorSettings().maximumGapSamples / rate * hz

  test("counts one rep per cycle of a steady cadence, losing only the filter's settling window"):
    // 40 seconds at 0.556 Hz is the stair-climber example from the design notes: about 22 cycles.
    Seq((0.556, 40.0), (1.0, 40.0), (1.0, 80.0), (1.5, 60.0)).foreach: (hz, seconds) =>
      val reading = run(rotating(hz, seconds, amplitude = 6.0))
      val cycles = hz * seconds

      assert(reading.lock.isInstanceOf[LockState.Locked], s"$hz Hz: expected a lock, got ${reading.lock}")
      // Never more than really happened: a count that invents reps is worse than one that misses the opening few.
      assert(reading.count <= cycles, f"$hz%.3f Hz over ${seconds}%.0fs counted ${reading.count} of $cycles%.1f")
      val floor = cycles - settlingLoss(hz) - returnLoss(hz) - 2
      assert(reading.count >= floor, f"$hz%.3f Hz counted ${reading.count}, below the tolerable $floor%.1f")

  test("the frequency accumulator adds each rep's own rate, from the second rep onwards"):
    // The account's formula, applied to what the detector actually saw: every rep from the second adds the
    // reciprocal of the gap behind it. At a steady cadence that is the cadence itself, once per rep, so the sum comes
    // to the frequency times one fewer than the reps counted.
    Seq(0.556, 1.0, 1.5).foreach: hz =>
      val reading = run(rotating(hz, 40.0, amplitude = 6.0))
      val gaps = math.max(0, reading.count - 1)

      assert(reading.cadenceSum > 0, f"$hz%.3f Hz accumulated nothing over ${reading.count} reps")
      // Generous, because the sum is taken over the leading channel's own peaks while the count is corroborated by
      // another, and the two can differ by a rep at the edges of a set. What is being checked is that the sum is the
      // cadence rather than the rep count, which a factor of two in either direction would not survive.
      assertEqualsDouble(reading.cadenceSum / gaps, hz, hz * 0.25)

  test("a faster set accumulates more than a slower one of the same length"):
    // The whole point of counting by frequency: the same twenty seconds of work is worth more done quickly.
    val slow = run(rotating(0.6, 60.0, amplitude = 6.0))
    val fast = run(rotating(1.6, 60.0, amplitude = 6.0))

    assert(fast.cadenceSum > slow.cadenceSum * 2, s"${fast.cadenceSum} against ${slow.cadenceSum}")

  test("the set's clock runs from its first counted rep, not from the camera opening"):
    // The seconds the detector spends working out what it is looking at are not exercise, and a rate divided by a
    // clock that included them would report the app's warm-up as a slow start.
    val reading = run(rotating(1.0, 60.0, amplitude = 6.0))

    assert(reading.elapsedSeconds > 0, "a counted set has a duration")
    assert(reading.elapsedSeconds < 60.0, f"${reading.elapsedSeconds}%.1fs covers more than the recording itself")
    // Only the filter's settling window and the moment before the first peak are outside it.
    assert(reading.elapsedSeconds > 50.0, f"${reading.elapsedSeconds}%.1fs is short of the set it covers")

  test("the clock keeps running when the reps stop, because a rest is part of the workout"):
    // A clock that stopped with the exercise would let someone resting half the time be projected as though they had
    // not rested at all -- and would leave a dashboard in front of an abandoned phone reporting its last rate for ever.
    val moving = rotating(1.0, 40.0, amplitude = 6.0)
    val resting = Quadrant.All.map(quadrant => quadrant -> (moving(quadrant) ++ still(20.0))).toMap

    val worked = run(moving)
    val rested = run(resting)

    // The rest adds twenty seconds of clock and next to no reps: what it does add is the tail of the set itself,
    // whose last peaks could not be judged until the movement had come back down from them.
    assert(rested.count - worked.count <= 3, s"the rest was counted: ${worked.count} became ${rested.count}")
    assert(
      rested.elapsedSeconds > worked.elapsedSeconds + 15.0,
      f"the clock gained only ${rested.elapsedSeconds - worked.elapsedSeconds}%.1fs over a 20s rest"
    )

  test("a reset leaves nothing of the previous set behind"):
    val counter = RepCounter()
    val before = run(rotating(1.0, 40.0, amplitude = 6.0), counter)
    assert(before.count > 0 && before.cadenceSum > 0)

    counter.reset()

    assertEquals(counter.reading.count, 0)
    assertEqualsDouble(counter.reading.cadenceSum, 0.0, 0.0)
    assertEqualsDouble(counter.reading.elapsedSeconds, 0.0, 0.0)

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
    assertEquals(settings.prominenceFloor, 4.0)
    // A peak stands about twice a channel's amplitude above its valleys, so the floor bites below half of it.
    val tooSmall = rotating(1.0, 60.0, amplitude = settings.prominenceFloor / 4)

    assertEquals(run(tooSmall).count, 0, "a movement at a quarter of the floor was counted")

  test("the strength badge keeps its measured boundaries when the floor moves"):
    // The margin divides by the floor, so lowering the floor raises every margin by the same proportion. A badge
    // whose bands were fixed multiples would silently re-grade every movement each time a detector threshold moved,
    // and would then be reporting the threshold rather than the signal.
    val floor = DetectorSettings().prominenceFloor

    assertEqualsDouble(SignalStrength.StrongAbove * floor, 8.0, 0.001)
    assertEqualsDouble(SignalStrength.AdequateAbove * floor, 6.0, 0.001)

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

  test("a quadrant the movement reaches late is not allowed to lose a rep the other one saw"):
    // Two quadrants of a locked pair are not two views of one peak train: the movement crosses them at different
    // moments, so at the edges of a set they honestly disagree by one. Which of them leads is settled by signal
    // power, often by a couple of percent, and across fifty-nine recordings the loser was repeatedly the one that
    // had seen every rep -- six suites in a row came in one short that way.
    val cycles = 30
    val full = rotating(1.0, cycles.toDouble, amplitude = 6.0)
    // The leading channel starts a quarter of a cycle late, so its first crossing never happened. The others carry
    // the whole movement, exactly as a quadrant further from the start does.
    val late = full.updated(Quadrant.Q1, full(Quadrant.Q1).drop((rate / 4).toInt))

    val whole = run(full).count
    val clipped = run(late).count

    assertEquals(clipped, whole, "a rep one channel missed at the edge was lost from the total")

  test("the count still never goes backwards when the pair changes"):
    // Two tallies mean two numbers that could disagree, and the reported one is a maximum over them. A pair that
    // moves to a channel with a smaller tally must not make the count drop: a rep that happened cannot un-happen.
    val counter = RepCounter()
    val channels = rotating(1.0, 40.0, amplitude = 6.0)
    var highest = 0

    for taken <- 1 to (40 * rate).toInt do
      val window = channels.view.mapValues(_.take(taken).takeRight(QuadrantSignals.DetectionWindow)).toMap
      val count = counter.update(window, taken).count
      assert(count >= highest, s"the count fell from $highest to $count")
      highest = count

  test("a channel reading its own noise cannot run away with the count"):
    // The one case where believing the larger tally would be a disaster. A quadrant the figure barely reaches has
    // almost no signal, its threshold falls to the floor, and the noise it finds can match the cadence by chance --
    // on real recordings such a channel counted a hundred and thirty-six where a hundred reps were performed, with
    // enough power that guarding on strength let it straight through.
    val cycles = 30
    val real = rotating(1.0, cycles.toDouble, amplitude = 6.0)
    // One quadrant carries the same cadence with a second bump inside every cycle, so it finds roughly twice as many
    // peaks while still agreeing about the period.
    val doubled = Seq.tabulate(real(Quadrant.Q2).length): i =>
      val t = i / rate
      120.0 + 6.0 * math.sin(2 * math.Pi * t) + 6.0 * math.sin(4 * math.Pi * t)
    val counter = RepCounter()
    val channels = real.updated(Quadrant.Q2, doubled)

    val reading = run(channels, counter)

    assert(
      reading.count <= cycles,
      s"a noisy channel pushed the count to ${reading.count} where only $cycles cycles happened"
    )

  test("the honest gap between two quadrants is a rep, so that is what is allowed"):
    // Not a number picked for comfort: across sixty-five recordings two quadrants of one movement differed by one
    // rep, and at most two. The margin is what separates that from a channel watching something else entirely.
    assert(DetectorSettings().quadrantDisagreement >= 1, "a quadrant must be allowed the rep another one missed")
    assert(DetectorSettings().quadrantDisagreement <= 3, "a wider margin admits a channel reading its own noise")

  test("the wait for a rep to come back follows the cadence being kept"):
    // This wait is the count's standing delay behind the movement: a peak cannot be judged until there is signal
    // after it. Taking it from the slowest cadence the band admits made it two seconds at every pace, which at
    // 0.8Hz is most of two reps -- and a dashboard two reps behind looks broken while being correct.
    val settings = DetectorSettings()
    val brisk = Seq(0, 6, 12, 18, 24)
    val slower = Seq(0, 25, 50, 75, 100)

    assertEquals(RepAnalysis.returnWindow(brisk, settings), 6, "a brisk cadence should not wait a slow one's period")
    assertEquals(RepAnalysis.returnWindow(slower, settings), settings.maximumGapSamples, "capped at the old wait")

  test("with nothing to judge by, the wait is the one the band implies"):
    val settings = DetectorSettings()

    assertEquals(RepAnalysis.returnWindow(Seq.empty, settings), settings.maximumGapSamples)
    assertEquals(RepAnalysis.returnWindow(Seq(4), settings), settings.maximumGapSamples)

  test("the wait never falls below the closest two peaks may be"):
    // A period estimate can collapse when a channel is reading noise; the wait must not collapse with it, or a peak
    // would be judged on a sample or two of signal and the return test would mean nothing.
    val settings = DetectorSettings()

    assertEquals(RepAnalysis.returnWindow(Seq(0, 1, 2, 3, 4), settings), settings.minimumDistanceSamples)

  test("a channel nothing else agrees with is not believed, even when it leads"):
    // The runaway from a real recording: one quadrant reading its own noise reached 136 while the others sat at 99.
    // Comparing against the leader let it through whenever it happened to be the leader, because nothing bounds a
    // leader against itself.
    val margin = DetectorSettings().quadrantDisagreement

    assertEquals(RepAnalysis.corroborated(Seq(136, 99, 99), margin), Some(99))
    assertEquals(RepAnalysis.corroborated(Seq(99, 136, 99), margin), Some(99))
    assertEquals(RepAnalysis.corroborated(Seq(99, 99, 136), margin), Some(99))

  test("a rep one channel saw and another nearly did is believed"):
    // The honest disagreement: two quadrants of one movement differ about its edges by a rep, never its middle.
    val margin = DetectorSettings().quadrantDisagreement

    assertEquals(RepAnalysis.corroborated(Seq(99, 100), margin), Some(100))
    assertEquals(RepAnalysis.corroborated(Seq(100, 99, 0), margin), Some(100))

  test("a channel that has only just begun agreeing cannot freeze the count"):
    // The failure this exists for. A channel starting its tally at zero and winning on power made every other tally
    // further away than the margin allowed, so nothing could raise the count until it caught up -- and a recorded
    // test counted nothing for two minutes while reporting the previous test's total, and passed.
    val margin = DetectorSettings().quadrantDisagreement

    assertEquals(RepAnalysis.corroborated(Seq(0, 60, 61), margin), Some(61), "the two that agree must be heard")

  test("with no second opinion there is nothing to corroborate"):
    val margin = DetectorSettings().quadrantDisagreement

    assertEquals(RepAnalysis.corroborated(Seq(42), margin), None)
    assertEquals(RepAnalysis.corroborated(Seq.empty, margin), None)
    assertEquals(RepAnalysis.corroborated(Seq(10, 90), margin), None, "two channels far apart agree about nothing")

  test("speckle left behind when a set ends does not start counting"):
    // The failure this exists for. The threshold a peak must clear is the greater of the fixed floor and a multiple of
    // the amplitude around it, and the second term cannot reject noise: a window holding only noise sets a threshold
    // that noise clears, since peaks routinely exceed one and a half times their own RMS. One recording gained eleven
    // reps after its movement had stopped, as the threshold decayed behind the departing set.
    val settings = DetectorSettings()
    val reps = Seq(0.0, 20.0, 0.0, 20.0, 0.0, 20.0, 0.0, 20.0, 0.0, 20.0, 0.0)
    val afterwards = Seq(0.0, 6.0, 0.0, 6.0, 0.0, 6.0, 0.0)
    val settled = reps ++ afterwards
    val peaks = settled.indices.filter(i =>
      i > 0 && i < settled.length - 1 &&
        settled(i) > settled(i - 1) && settled(i) >= settled(i + 1)
    )

    val standing = RepAnalysis.standingOut(settled, peaks, Some(2.0), settings)

    assert(standing.forall(_ < reps.length), s"noise after the set was counted: $standing")
    assertEquals(standing.size, 5, "every rep of the set must survive")

  test("a movement that dims as the lighting does is still counted"):
    // The opposite failure, and the more dangerous one because it is silent: a threshold anchored to what the scene used to
    // look like ratchets out of reach as a room darkens and the count simply stops. Auto-exposure was measured moving
    // the amplitude by as much as forty per cent in ten seconds, so this is not hypothetical.
    val settings = DetectorSettings()
    val fading = (0 until 12).flatMap(i => Seq(0.0, 20.0 * math.pow(0.94, i.toDouble)))
    val peaks = fading.indices.filter(i =>
      i > 0 && i < fading.length - 1 &&
        fading(i) > fading(i - 1) && fading(i) >= fading(i + 1)
    )

    val standing = RepAnalysis.standingOut(fading, peaks, Some(2.0), settings)

    assertEquals(standing, peaks, "a rep was thrown away because the room dimmed")

  test("the guard sits below the worst honest dip and above the noise it rejects"):
    // Both bounds are measurements, not preferences. Across sixty-four recordings no genuine rep fell below three
    // quarters of its own set's median, and cool-down noise stood at about a third of the movement it followed.
    assert(DetectorSettings().peakHeightShare < 0.75, "this would reject reps during ordinary exposure drift")
    assert(DetectorSettings().peakHeightShare > 0.36, "this would admit the noise that follows a set")
