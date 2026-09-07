package sgrv.fe.acquire

/** A fixed-capacity ring of samples that overwrites its oldest entry once full.
  *
  * The acquisition design calls for a continuously sliding buffer of raw signal that can be re-analysed when a regime
  * change is suspected, rather than a single irreversible judgement made live. This is that buffer: it never
  * allocates after construction and never grows, so it can be fed at 10 Hz indefinitely.
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
private[fe] final class QuadrantSignals(val capacity: Int = QuadrantSignals.OneMinute):
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
  /** One minute at the 10 Hz sampling rate. */
  val OneMinute = 600
