package sgrv.fe.acquire

/** Tuning for the detector. Every threshold here was reasoned about rather than measured, and the acquisition design
  * expects them to be settled empirically; they are gathered in one place so that can happen.
  */
private[fe] final case class DetectorSettings(
    sampleRateHz: Double = 10.0,
    lowHz: Double = 0.5,
    highHz: Double = 2.0,
    /** Two hertz is the fastest cadence considered possible, so half a second is the closest two reps may fall. */
    minimumDistanceSamples: Int = 5,
    /** A peak must stand this far above the surrounding valleys, relative to how active the window is. */
    prominenceFactor: Double = 1.5,
    /** and at least this far in absolute terms, so a still scene's noise cannot clear a threshold that scales with it
      * and be counted as movement.
      */
    prominenceFloor: Double = 1.0,
    /** Fifteen seconds of samples before a lock is attempted: enough to hold several cycles of the slowest cadence in
      * the band.
      */
    minimumSamplesForLock: Int = 150,
    /** How closely two channels' periods must match to be believed. */
    periodTolerance: Double = 0.25,
    /** How many peaks a channel must show before it may claim a period at all.
      *
      * Incidental movement — someone shifting in a chair, walking past — produces peaks as large as a rep's, so
      * amplitude cannot tell the two apart. What distinguishes exercise is that it repeats. Requiring several peaks
      * means a channel has to show a cadence, not merely a couple of events.
      */
    minimumPeaksForPeriod: Int = 4,
    /** How far an individual gap between peaks may stray from the typical one, as a fraction of it.
      *
      * A cadence is even. Peaks spaced 8, 23 and 11 samples apart have a median like a real signal's but are plainly
      * not rhythmic, and this is what rejects them.
      */
    maximumIntervalSpread: Double = 0.35,
    /** How many of the most recent peaks describe the cadence being kept now.
      *
      * Judging evenness across the whole buffer means a single pause poisons it until that pause scrolls out of the
      * minute entirely — a minute of not counting after movement resumes. Looking only at the recent peaks makes the
      * period what the user is doing now, so a pause is forgotten after a handful of reps rather than after the buffer
      * turns over. It also lets the cadence change without the earlier pace arguing against it.
      */
    cadencePeaks: Int = 5,
    /** Samples ignored at the start of a window while the filter settles. A band-pass starting from rest rings when the
      * signal first arrives, and that ringing would otherwise dominate the very statistics used to decide what counts
      * as a peak.
      */
    settlingSamples: Int = 20
):
  val band: Biquad = Biquad.bandPass(lowHz, highHz, sampleRateHz)

/** Whether the detector currently trusts what it is measuring. Mirrors a phase-locked loop's lock detector: locked
  * means the output can be believed, anything else means say so rather than guess.
  */
private[fe] enum LockState:
  case Acquiring(samples: Int, needed: Int)
  case Searching
  case Locked(channel: Quadrant, partner: Quadrant, periodSeconds: Double)

private[fe] final case class ChannelAnalysis(
    quadrant: Quadrant,
    filtered: Seq[Double],
    power: Double,
    peaks: Seq[Int],
    periodSamples: Option[Double]
)

private[fe] final case class RepReading(count: Int, lock: LockState)

private[fe] object RepAnalysis:

  private[acquire] def rootMeanSquare(samples: Seq[Double]): Double =
    if samples.isEmpty then 0.0 else math.sqrt(samples.map(value => value * value).sum / samples.size)

  /** The cadence a channel is keeping, in samples per rep, or `None` if it is not keeping one.
    *
    * The median rather than the mean, so one missed or spurious peak does not drag the estimate — but a median alone
    * would describe any set of peaks whatsoever. A period is only claimed when there are enough peaks to call it a
    * rhythm and every gap between them resembles the others.
    */
  private[acquire] def periodOf(
      allPeaks: Seq[Int],
      minimumPeaks: Int,
      maximumSpread: Double,
      cadencePeaks: Int
  ): Option[Double] =
    // Only the recent peaks: the period is the cadence being kept now, not an average of everything remembered.
    val peaks = allPeaks.takeRight(math.max(cadencePeaks, minimumPeaks))
    val intervals = peaks.sliding(2).collect { case Seq(first, second) => (second - first).toDouble }.toSeq
    if peaks.sizeIs < minimumPeaks || intervals.isEmpty then None
    else
      val sorted = intervals.sorted
      val middle = sorted.length / 2
      val median = if sorted.length % 2 == 1 then sorted(middle) else (sorted(middle - 1) + sorted(middle)) / 2
      val even = median > 0 && intervals.forall(interval => math.abs(interval - median) / median <= maximumSpread)
      Option.when(even)(median)

  def analyse(samples: Seq[Double], quadrant: Quadrant, settings: DetectorSettings): ChannelAnalysis =
    // Centring first: the band-pass rejects a constant in steady state, but a filter starting from rest still sees
    // the signal's resting level as a step and rings on it. Brightness sits around 120, so that transient would
    // dwarf a rep of a few units and drag the prominence threshold up with it.
    val mean = if samples.isEmpty then 0.0 else samples.sum / samples.size
    val filtered = settings.band.filter(samples.map(_ - mean))
    val settled = filtered.drop(settings.settlingSamples)
    val power = rootMeanSquare(settled)
    val threshold = math.max(settings.prominenceFloor, settings.prominenceFactor * power)
    // Peaks are reported against the whole window, so the caller's index arithmetic stays unaffected by the skip.
    val peaks = PeakDetector
      .peaks(settled, settings.minimumDistanceSamples, threshold)
      .map(_ + settings.settlingSamples)
    ChannelAnalysis(
      quadrant,
      filtered,
      power,
      peaks,
      periodOf(peaks, settings.minimumPeaksForPeriod, settings.maximumIntervalSpread, settings.cadencePeaks)
    )

  private[acquire] def agree(first: ChannelAnalysis, second: ChannelAnalysis, tolerance: Double): Boolean =
    (first.periodSamples, second.periodSamples) match
      case (Some(a), Some(b)) if a > 0 && b > 0 => math.abs(a - b) / math.max(a, b) <= tolerance
      case _                                    => false

  /** The strongest pair of channels whose periods agree, with the stronger of the two leading.
    *
    * The acquisition design settled on this rather than a vote across all four, because a movement may only cross two
    * quadrants: the condition is that two agree, not that most do. Peak timing is then taken from one channel alone —
    * the quadrants carry the same period but different phase, so fusing their peaks would smear the timing the count
    * depends on.
    */
  /** Keeps a channel that is already leading, paired with whichever other channel best agrees with it. */
  def hold(
      channels: Seq[ChannelAnalysis],
      quadrant: Quadrant,
      tolerance: Double
  ): Option[(ChannelAnalysis, ChannelAnalysis)] =
    for
      leader <- channels.find(_.quadrant == quadrant)
      partner <- channels.filter(_.quadrant != quadrant).filter(agree(leader, _, tolerance)).maxByOption(_.power)
    yield (leader, partner)

  def select(channels: Seq[ChannelAnalysis], tolerance: Double): Option[(ChannelAnalysis, ChannelAnalysis)] =
    val pairs = for
      first <- channels
      second <- channels
      if first.quadrant.ordinal < second.quadrant.ordinal
      if agree(first, second, tolerance)
    yield if first.power >= second.power then (first, second) else (second, first)
    pairs.sortBy((leader, partner) => -(leader.power + partner.power)).headOption

/** Counts reps from the confirmed peaks of whichever channel is currently trusted.
  *
  * The count only ever rises. Each update re-detects peaks over the buffered window, but a peak already counted stays
  * counted even if a later, longer view of the signal would no longer pick it — a rep that happened cannot un-happen,
  * and a count that went backwards would be worse than one that is slightly generous.
  *
  * Nothing is ever inferred from elapsed time. When a lock is first established the whole buffer is counted, so the
  * reps performed while the detector was still deciding are not lost: that is a delayed reveal of peaks genuinely
  * found, not a backfill of peaks assumed.
  */
private[fe] final class RepCounter(settings: DetectorSettings = DetectorSettings()):
  private var counted = 0
  private var lastCountedIndex: Option[Int] = None
  private var authoritative: Option[Quadrant] = None
  private var state: LockState = LockState.Acquiring(0, settings.minimumSamplesForLock)

  def reading: RepReading = RepReading(counted, state)

  /** Sets the tally back to nothing without disturbing the detection behind it.
    *
    * Deliberately not a reset: the lock, the channel being counted from and the position of the last counted peak all
    * stand, so a set already in progress keeps being counted and simply starts from zero. Wiping the position would
    * make the next update rediscover every peak still in the buffer and count them all over again, and wiping the
    * buffers would throw away the cadence and cost several reps re-acquiring it.
    */
  def zeroCount(): Unit =
    counted = 0

  def reset(): Unit =
    counted = 0
    lastCountedIndex = None
    authoritative = None
    state = LockState.Acquiring(0, settings.minimumSamplesForLock)

  /** Folds one window of per-quadrant samples into the running count.
    *
    * `totalSamples` is how many have ever been recorded, which turns a position inside the window into an identity that
    * survives the buffer wrapping — without it, a peak would be recounted every time the window slid.
    */
  def update(window: Map[Quadrant, Seq[Double]], totalSamples: Int): RepReading =
    val lengths = window.values.map(_.size)
    val windowLength = if lengths.isEmpty then 0 else lengths.min

    if windowLength < settings.minimumSamplesForLock then
      state = LockState.Acquiring(windowLength, settings.minimumSamplesForLock)
    else
      val channels = window.toSeq.map((quadrant, samples) => RepAnalysis.analyse(samples, quadrant, settings))
      // Stay with the channel already being counted for as long as it still agrees with another. The quadrants
      // carry the same period at different phases, so a leader that changed between updates would interleave two
      // phases of the same movement and count each cycle more than once.
      val held = authoritative.flatMap(quadrant => RepAnalysis.hold(channels, quadrant, settings.periodTolerance))
      held.orElse(RepAnalysis.select(channels, settings.periodTolerance)) match
        case None =>
          state = LockState.Searching
          authoritative = None
        case Some((leader, partner)) =>
          authoritative = Some(leader.quadrant)
          state = LockState.Locked(
            leader.quadrant,
            partner.quadrant,
            leader.periodSamples.getOrElse(0.0) / settings.sampleRateHz
          )
          val absolute = leader.peaks.map(index => totalSamples - windowLength + index)
          val fresh = lastCountedIndex match
            case None => absolute
            // A peak must clear the last counted one by the minimum rep interval, not merely come after it.
            // Re-detecting over a window that has grown or slid can move a peak by a sample, and "later than the
            // last" would then count that same peak a second time.
            case Some(last) => absolute.filter(_ >= last + settings.minimumDistanceSamples)
          if fresh.nonEmpty then
            counted += fresh.size
            lastCountedIndex = Some(fresh.max)

    reading
