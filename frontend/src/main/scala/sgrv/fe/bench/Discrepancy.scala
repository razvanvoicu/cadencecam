package sgrv.fe.bench

/** How the counter under test compares with what was actually shown to it. */
private[fe] final case class Comparison(reference: Int, acquired: Int, lagSeconds: Double, withinTolerance: Boolean):
  def behind: Boolean = lagSeconds > 0
  def ahead: Boolean = lagSeconds < 0

private[fe] object Discrepancy:

  /** Compares the two counts in seconds rather than in reps.
    *
    * A shortfall of one rep means something different at every cadence -- two seconds at half a hertz, half a second at
    * two -- so a tolerance expressed in reps would be strict in slow tests and lax in fast ones. Measuring how long the
    * counter has been wrong makes one threshold mean the same thing everywhere.
    *
    * Positive lag is behind: the reference passed this count some seconds ago and the counter has not caught up.
    * Negative is ahead: the counter has claimed a rep that will not finish for some seconds yet.
    */
  def compare(cadence: Cadence, elapsedSeconds: Double, acquired: Int, tolerance: Double): Comparison =
    val reference = cadence.repsBy(elapsedSeconds)
    val lag =
      if acquired < reference then elapsedSeconds - cadence.timeOfRep(acquired + 1)
      else if acquired > reference then elapsedSeconds - cadence.timeOfRep(acquired)
      else 0.0
    Comparison(reference, acquired, lag, math.abs(lag) <= tolerance)

/** Watches a counter that has stopped moving, and reports what happened when it starts again.
  *
  * The case worth catching is not a counter that is simply wrong but one that stalls and then recovers: whether the
  * reps performed during the stall are eventually credited, silently lost, or made up for with interest, is the
  * question this whole harness exists to answer, and it cannot be seen from the final total alone.
  */
private[fe] final class StallWatch:
  private var lastAcquired = 0
  private var stalledFrom = Option.empty[Double]
  private var referenceAtStall = 0

  /** Feeds in one observation, returning an event worth recording if this is a moment worth recording. */
  def observe(comparison: Comparison, elapsedSeconds: Double): Option[StallWatch.Event] =
    if comparison.acquired != lastAcquired then
      val recovered = stalledFrom.map: began =>
        val expected = comparison.reference - referenceAtStall
        val credited = comparison.acquired - lastAcquired
        StallWatch.Recovered(
          stalledSeconds = elapsedSeconds - began,
          repsDuringStall = expected,
          repsCredited = credited,
          // The number that matters: whether the stall cost reps, or invented them.
          shortfall = expected - credited
        )
      lastAcquired = comparison.acquired
      stalledFrom = None
      recovered
    else if stalledFrom.isEmpty && comparison.behind && !comparison.withinTolerance then
      stalledFrom = Some(elapsedSeconds - comparison.lagSeconds)
      referenceAtStall = comparison.acquired
      Some(StallWatch.Stalled(comparison.lagSeconds))
    else None

private[fe] object StallWatch:
  sealed trait Event
  final case class Stalled(behindSeconds: Double) extends Event
  final case class Recovered(
      stalledSeconds: Double,
      repsDuringStall: Int,
      repsCredited: Int,
      shortfall: Int
  ) extends Event
