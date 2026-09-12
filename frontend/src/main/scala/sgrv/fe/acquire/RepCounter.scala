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
    /** and at least this far in absolute terms, so a still scene cannot clear a threshold that scales with it.
      *
      * Set from what actually varies at rest rather than from sensor noise alone. Per-pixel noise averages away -- a
      * quadrant's brightness is the mean of some six hundred pixels -- but auto-exposure hunting, mains flicker and the
      * camera's own denoising move the whole frame together and survive that average intact, which is why a floor set
      * for sensor noise alone was far too low.
      *
      * The cost is a floor on how small a rep may be: a peak stands about twice a channel's amplitude above its
      * valleys, so this rejects any movement swinging a quadrant's brightness by less than half of it.
      *
      * Four rather than five. "About twice the amplitude" holds only for a peak with movement on both sides, and the
      * first rep of a set has none: it rises out of stillness, so its prominence is measured against a resting level
      * rather than a trough and comes to about half what its neighbours score. One recording missed its opening rep on
      * exactly that margin -- 4.27 against a floor of 5, where every later crossing scored 7.2 to 7.8, with the same
      * excursion in the raw brightness and the same fall afterwards.
      *
      * It is also less load-bearing than it was. The floor was raised to five when prominence was the only test of
      * amplitude, and camera shake was measured scoring up to thirteen, so it was never what rejected shake; a peak
      * must now also be one the movement came back from, which is the test that does. Replayed over fifty-three
      * recordings on the current bench, four recovered two counts and lost none, and nothing ran away -- the worst
      * count anywhere was a hundred and three, through fifteen seconds of a deliberately static scene.
      */
    prominenceFloor: Double = 4.0,
    /** Fifteen seconds of samples before a lock is attempted: enough to hold several cycles of the slowest cadence in
      * the band.
      */
    minimumSamplesForLock: Int = 150,
    /** How closely two channels' periods must match to be believed. */
    periodTolerance: Double = 0.25,
    /** How many peaks in a row make a sustained movement.
      *
      * Exercise is not one event but a stream of them. An isolated peak, or two, is someone shifting in a chair or
      * reaching for a towel; the same peak arriving again and again is a set. Until this many have accumulated nothing
      * is counted, and when the last of them arrives they all count together -- each one is evidence for the others,
      * and none of them was a rep on its own.
      */
    minimumSustainedPeaks: Int = 5,
    /** How many recent reps the reported pace is measured over.
      *
      * Ten is long enough that one slow rep does not swing the figure and short enough to follow a real change of pace
      * within a set, rather than reporting an average of a workout that has moved on.
      */
    paceWindowReps: Int = 10,
    /** How far a peak's own quadrant must come back down after it, against how far the movement usually comes back.
      *
      * A rep is a round trip and a weight being put down is not, but a band-pass cannot tell them apart: it is blind to
      * a constant, so a brightness that rose and stayed reads exactly like one that rose and fell. Every recorded test
      * with a dark object on a light ground ended with a phantom rep at the moment the object left the frame, and this
      * is what rejects it.
      *
      * A fifth. Across twenty-four recorded tests every real rep came back at least a third as far as the typical one,
      * and every phantom less than a seventh; a fifth sits between, half again above the worst phantom and a third
      * below the weakest real rep.
      */
    returnFraction: Double = 0.2,
    /** How far a quadrant's tally may exceed the leading one's and still be believed.
      *
      * Two quadrants watching one movement disagree about its edges, never about its middle: the figure reaches them at
      * different moments, so at the start and the end of a set one of them can legitimately have seen a crossing the
      * other did not. Measured over sixty-five recordings that honest gap was one rep, and at most two.
      *
      * A channel claiming twenty-five more is not watching the same movement, whatever its period says. That happens: a
      * quadrant the figure barely reaches has almost no signal, its threshold falls to the floor, and the noise it then
      * finds can match the cadence by chance. Its power is no help in spotting it -- guarding on that was tried and let
      * every one of them through -- but the size of its claim gives it away at once.
      */
    quadrantDisagreement: Int = 2,
    /** Samples ignored at the start of a window while the filter settles. A band-pass starting from rest rings when the
      * signal first arrives, and that ringing would otherwise dominate the very statistics used to decide what counts
      * as a peak.
      */
    settlingSamples: Int = 20
):
  val band: Biquad = Biquad.bandPass(lowHz, highHz, sampleRateHz)

  /** The longest gap that still belongs to the same sustained movement.
    *
    * Derived rather than chosen: it is the period of the slowest cadence the band-pass admits, so a gap too long to be
    * a rep at any pace this detector can see is also too long to hold a group together. Widening the band later moves
    * this with it instead of leaving the two quietly disagreeing.
    */
  val maximumGapSamples: Int = math.round(sampleRateHz / lowHz).toInt

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
    periodSamples: Option[Double],
    /** How far a typical peak here stood above the bar it had to clear, as a multiple of that bar.
      *
      * One is a movement only just distinguishable from the background; measured runs that counted correctly sat above
      * three, and one that lost reps sat under two. `None` when there were no peaks to judge.
      */
    margin: Option[Double]
)

private[fe] final case class RepReading(count: Int, lock: LockState, margin: Option[Double] = None)

private[fe] object RepAnalysis:

  private[acquire] def rootMeanSquare(samples: Seq[Double]): Double =
    if samples.isEmpty then 0.0 else math.sqrt(samples.map(value => value * value).sum / samples.size)

  /** Median interval between successive peaks, in samples. The median rather than the mean so one missed or spurious
    * peak does not drag the estimate.
    */
  private[acquire] def periodOf(peaks: Seq[Int]): Option[Double] =
    val intervals = peaks.sliding(2).collect { case Seq(first, second) => (second - first).toDouble }.toSeq
    Option.when(intervals.nonEmpty):
      val sorted = intervals.sorted
      val middle = sorted.length / 2
      if sorted.length % 2 == 1 then sorted(middle) else (sorted(middle - 1) + sorted(middle)) / 2

  /** The peaks that belong to a sustained movement: runs of at least `minimumPeaks`, each no further from the last than
    * `maximumGap`. Peaks outside such a run are dropped, however prominent they were.
    *
    * Read over the whole buffer on every update, so a run that has only reached three peaks is simply not counted yet
    * rather than counted and later regretted. When its fourth arrives the whole run becomes countable at once.
    *
    * This is a separate question from whether a cadence is locked, and both must be satisfied: the lock asks whether
    * two quadrants agree about a period, this asks whether the movement kept going.
    */
  private[acquire] def sustained(peaks: Seq[Int], minimumPeaks: Int, maximumGap: Int): Seq[Int] =
    val sorted = peaks.sorted
    var runs = Vector.empty[Vector[Int]]
    var current = Vector.empty[Int]
    for peak <- sorted do
      if current.isEmpty || peak - current.last <= maximumGap then current = current :+ peak
      else
        runs = runs :+ current
        current = Vector(peak)
    if current.nonEmpty then runs = runs :+ current
    runs.filter(_.sizeIs >= minimumPeaks).flatten

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
    val measured = PeakDetector.measured(settled, settings.minimumDistanceSamples, threshold)
    // Against the quadrant's own brightness, so a peak the movement never came back from -- the object leaving the
    // frame for good -- is not mistaken for the round trip a rep is.
    val found = measured.map(_._1 + settings.settlingSamples)
    // Judged over the time one rep takes here rather than the slowest the band allows: that wait is the count's
    // standing delay behind the movement, so where it comes from is not a detail.
    val peaks = PeakDetector.returning(samples, found, returnWindow(found, settings), settings.returnFraction)
    // Of the peaks that survived, not of everything the detector looked at: this says how far the movement being
    // counted stands above the bar, and a peak that was rejected is not being counted.
    val kept = peaks.toSet
    val prominences = measured.collect {
      case (index, prominence) if kept.contains(index + settings.settlingSamples) => prominence
    }.sorted
    // Against the fixed floor rather than the adaptive threshold. The threshold rises with the scene's own
    // activity, so dividing by it flatters a quiet scene: measured against real recordings, a camera shaken by
    // typing scored higher that way than a movement that counted perfectly. The floor is the bar a movement
    // actually has to clear, and it does not move.
    val margin = Option.when(prominences.nonEmpty && settings.prominenceFloor > 0):
      prominences(prominences.size / 2) / settings.prominenceFloor
    ChannelAnalysis(quadrant, filtered, power, peaks, periodOf(peaks), margin)

  /** How long to wait for a peak's movement to come back: the time one rep takes here.
    *
    * A peak is held back until there is enough signal after it to judge, so this wait is the count's standing delay
    * behind the movement. Taking it from the slowest cadence the band admits made that delay two seconds whatever the
    * cadence, which at 0.8Hz is most of two reps -- and the trough a rep returns through arrives half a period after
    * its peak, so a whole period of the cadence actually being kept is already generous. Measured over sixty-five
    * recordings it brought the delay from 2.1s to 1.3s without changing a count.
    *
    * Bounded both ways: never shorter than the closest two peaks may fall, and never longer than the old fixed wait, so
    * an implausible period cannot make it wait longer than it used to.
    */
  private[acquire] def returnWindow(peaks: Seq[Int], settings: DetectorSettings): Int =
    periodOf(peaks)
      .map(period => period.round.toInt.max(settings.minimumDistanceSamples).min(settings.maximumGapSamples))
      .getOrElse(settings.maximumGapSamples)

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

  /** A tally per quadrant, each counted entirely from that quadrant's own peaks.
    *
    * The two channels of a locked pair are not two views of one peak train; they are two quadrants the movement crosses
    * at different moments, and at the edges of a set they honestly disagree about how many crossings they saw.
    * Whichever is picked to lead is decided by signal power, often by a couple of percent, and the loser has repeatedly
    * been the one that saw every rep.
    *
    * So both are kept. Melding their peaks was tried first and is the harder problem -- the phase between them has to
    * be estimated and it is never clean enough -- while two separate tallies need no alignment at all.
    */
  private var tallies = Map.empty[Quadrant, Int]
  // When the recent counted reps happened, in absolute sample positions, for reporting the pace being kept.
  private var recent = Vector.empty[Int]
  private var lastCounted = Map.empty[Quadrant, Int]
  private var authoritative: Option[Quadrant] = None
  private var state: LockState = LockState.Acquiring(0, settings.minimumSamplesForLock)

  /** How far the strongest channel's peaks stood above their threshold, whether or not a cadence was found.
    *
    * Kept even while searching, because that is when it is most worth knowing: a movement too faint to count looks from
    * the outside exactly like no movement at all, and this tells the two apart.
    */
  private var currentMargin: Option[Double] = None

  def reading: RepReading = RepReading(counted, state, currentMargin)

  /** The pace of the last few reps, in reps per minute, or zero when too few have been seen to say.
    *
    * Measured across the gaps between counted reps rather than from the detected period: the period is what the
    * detector believes the cadence to be, while this is what actually got counted, and when those disagree the second
    * is the honest one to show.
    */
  def repsPerMinute: Double =
    if recent.sizeIs < 2 then 0.0
    else
      val span = (recent.last - recent.head).toDouble
      if span <= 0 then 0.0 else 60.0 * settings.sampleRateHz * (recent.size - 1) / span

  def reset(): Unit =
    counted = 0
    tallies = Map.empty
    recent = Vector.empty
    lastCounted = Map.empty
    authoritative = None
    state = LockState.Acquiring(0, settings.minimumSamplesForLock)

  /** Folds one channel's fresh peaks into that channel's own tally, returning where it has counted up to. */
  private def tally(channel: ChannelAnalysis, windowLength: Int, totalSamples: Int): Unit =
    // Alongside the lock rather than instead of it: the lock says a cadence is being kept, this says the movement
    // was sustained. A peak has to satisfy both before it is a rep.
    val supported = RepAnalysis.sustained(channel.peaks, settings.minimumSustainedPeaks, settings.maximumGapSamples)
    val absolute = supported.map(index => totalSamples - windowLength + index)
    val fresh = lastCounted.get(channel.quadrant) match
      case None => absolute
      // A peak must clear the last counted one by the minimum rep interval, not merely come after it.
      // Re-detecting over a window that has grown or slid can move a peak by a sample, and "later than the
      // last" would then count that same peak a second time.
      case Some(last) => absolute.filter(_ >= last + settings.minimumDistanceSamples)
    if fresh.nonEmpty then
      tallies = tallies.updated(channel.quadrant, tallies.getOrElse(channel.quadrant, 0) + fresh.size)
      lastCounted = lastCounted.updated(channel.quadrant, fresh.max)
      if authoritative.contains(channel.quadrant) then
        recent = (recent ++ fresh.sorted).takeRight(settings.paceWindowReps)

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
      // From whichever channel is carrying the most, which is the best case the movement offers rather than an
      // average dragged down by the quadrants nothing happens in.
      currentMargin = channels.flatMap(_.margin).maxOption
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
          // Every channel that agrees with the leader on period, each counted from its own peaks -- not just the
          // single best partner. Which one is "best" is settled by signal power, often by a couple of per cent, and
          // it changes hands during a set; a channel only tallies while it is in the pair, so the one set aside
          // stops accumulating and its total is no longer comparable. Two quadrants both saw every rep of one
          // recording and the count still came out short, because neither held the partner's place throughout.
          val agreeing = channels.filter: channel =>
            channel.quadrant == leader.quadrant || RepAnalysis.agree(leader, channel, settings.periodTolerance)
          agreeing.foreach(tally(_, windowLength, totalSamples))
          // The largest believable tally, and never less than what has already been reported.
          //
          // Largest rather than an average or a vote, because a quadrant that does not really see the movement finds
          // fewer crossings, not more: across sixty-five recordings a partner ran as much as a hundred reps below the
          // leader and never more than two above. Believable is what the margin decides -- a channel claiming far
          // more is reading its own noise, and that is the one case where taking the larger would be a disaster.
          val leading = tallies.getOrElse(leader.quadrant, 0)
          val believable = agreeing
            .map(channel => tallies.getOrElse(channel.quadrant, 0))
            .filter(_ <= leading + settings.quadrantDisagreement)
          counted = math.max(counted, believable.maxOption.getOrElse(leading))

    reading
