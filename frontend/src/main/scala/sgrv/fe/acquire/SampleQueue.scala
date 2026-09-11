package sgrv.fe.acquire

/** A fixed-capacity ring of samples that overwrites its oldest entry once full.
  *
  * The acquisition design calls for a continuously sliding buffer of raw signal that can be re-analysed when a regime
  * change is suspected, rather than a single irreversible judgement made live. This is that buffer: it never allocates
  * after construction and never grows, so it can be fed at 10 Hz indefinitely.
  */
private[fe] final class SampleQueue(val capacity: Int):
  require(capacity > 0, "a sample queue must hold at least one sample")

  private val values = Array.fill(capacity)(0.0)
  private var count = 0
  private var next = 0

  def size: Int = count
  def isFull: Boolean = count == capacity

  def add(value: Double): Unit =
    values(next) = value
    next = (next + 1) % capacity
    if count < capacity then count += 1

  /** Everything held, oldest first. */
  def toSeq: Seq[Double] = latest(count)

  /** The most recent `wanted` samples, oldest first; fewer if the queue has not filled that far yet. */
  def latest(wanted: Int): Seq[Double] =
    val taken = math.min(math.max(wanted, 0), count)
    val start = (next - taken + capacity) % capacity
    Seq.tabulate(taken)(index => values((start + index) % capacity))

  def clear(): Unit =
    count = 0
    next = 0

/** One ring per quadrant: the four parallel time series the detector will choose between. */
private[fe] final class QuadrantSignals(val capacity: Int = QuadrantSignals.Recorded):
  private val queues = Quadrant.All.map(quadrant => quadrant -> SampleQueue(capacity)).toMap

  def record(sample: Sample): Unit =
    Quadrant.All.foreach(quadrant => queues(quadrant).add(sample.brightness(quadrant)))

  def queue(quadrant: Quadrant): SampleQueue = queues(quadrant)

  def latest(quadrant: Quadrant, wanted: Int): Seq[Double] = queues(quadrant).latest(wanted)

  /** The same window from every quadrant, so the four can be compared instant by instant. */
  def window(wanted: Int): Map[Quadrant, Seq[Double]] =
    Quadrant.All.map(quadrant => quadrant -> queues(quadrant).latest(wanted)).toMap

  def size: Int = queues(Quadrant.Q1).size

  def clear(): Unit = queues.values.foreach(_.clear())

private[fe] object QuadrantSignals:
  /** How much of the signal the detector reasons over: fifteen seconds at the 10 Hz sampling rate.
    *
    * This window sets the bar as well as finding the peaks. The prominence threshold scales with the window's own
    * activity, so a minute-long window carried a whole minute of history into that bar: the last rep of a set went
    * uncounted for twenty to thirty seconds, waiting for the movement that preceded it to age out, and a test that
    * followed a stronger one spent its first half-minute failing to clear a threshold set by the previous test.
    * Measured against real recordings, fifteen seconds brought that delay down to seven and eleven seconds with the
    * counts unchanged at exactly a hundred.
    *
    * The floor under it is the sustained-movement rule: five peaks are needed before anything counts, and after the
    * filter's settling samples are dropped this window holds about six cycles of the slowest cadence the band admits.
    * Shortening it further would start rejecting slow exercise rather than stale history.
    *
    * Not the same quantity as how much is kept: the whole window is re-filtered and re-scanned for peaks on every
    * sample, so this also bounds the work done ten times a second on a phone.
    */
  val DetectionWindow = 150

  /** How much is kept, so a captured trace can show a whole test session rather than its last few seconds.
    *
    * Six minutes at ten hertz. Sized from what one bench test takes -- a hundred reps at 0.8Hz and the pause after it,
    * a little over two and a half minutes -- because a suite now captures and then resets after each test, so a
    * recording covers one test rather than the whole run. Keeping more costs memory and nothing else, since the
    * detector's window is separate and much shorter.
    */
  val Recorded = 3600
