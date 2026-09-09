package sgrv.fe.acquire

/** Works out where in a rep the counted tick actually lands.
  *
  * A brightness signal says how fast the movement repeats but not where a rep begins: "rep" is a fact about the
  * exerciser's intention, and a periodic waveform has no privileged starting phase. The detector picks its channel by
  * signal power, which follows the lighting, so the same curl counted from q4 ticks when the dumbbell is down and
  * counted from q2 ticks when it is up — half a rep apart, with nothing in the signal to say which is meant.
  *
  * The buffer supplies the missing anchor, after the fact. A set begins with the hand parked at rest, so the opening
  * stretch of the recording shows each quadrant at its resting brightness. The quadrant the hand rests in is the one
  * whose resting level sits at the *top* of the range it later sweeps through; a quadrant the hand only visits mid-rep
  * rests at the bottom of its own range. That is enough to say how far a channel's peak falls from the boundary the
  * exerciser is counting.
  *
  * Nothing here is live: it reads a record of what already happened, so it costs no latency and needs no prediction.
  */
private[fe] object RestPhase:

  /** How far a channel may drift and still count as motionless, in brightness units.
    *
    * Camera noise on a still scene is well under one unit; a rep swings several. This sits between the two.
    */
  val StillTolerance: Double = 1.5

  /** Shortest opening stillness worth trusting, in samples. Two seconds: long enough that a slow rep cannot be
    * mistaken for the hand being parked.
    */
  val MinimumStillSamples: Int = 20

  /** How much of a channel's range must be swept before "where rest sits in it" means anything. */
  val MinimumRange: Double = 3.0

  /** Length of the opening stretch in which every channel stayed within `tolerance` of where it started.
    *
    * All channels together, because the hand being at rest is one fact about the scene, not four: a quadrant the hand
    * never enters is motionless throughout and would otherwise report the whole recording as still.
    */
  def stillPrefix(channels: Map[Quadrant, Seq[Double]], tolerance: Double = StillTolerance): Int =
    val lengths = channels.values.map(_.size)
    if channels.isEmpty || lengths.isEmpty then 0
    else
      val starts = channels.view.mapValues(_.headOption.getOrElse(0.0)).toMap
      val limit = lengths.min
      var index = 0
      var moving = false
      while index < limit && !moving do
        if channels.exists((quadrant, samples) => math.abs(samples(index) - starts(quadrant)) > tolerance) then
          moving = true
        else index += 1
      index

  /** Where a channel's resting level sits within the range it sweeps once moving: 1 at its brightest, 0 at its
    * darkest, or `None` when there is not enough stillness or not enough movement to tell.
    */
  def restFraction(samples: Seq[Double], stillFor: Int): Option[Double] =
    val moving = samples.drop(stillFor)
    Option
      .when(stillFor >= MinimumStillSamples && moving.nonEmpty):
        val rest = samples.take(stillFor).sum / stillFor
        val lowest = moving.min
        val highest = moving.max
        Option.when(highest - lowest >= MinimumRange):
          // Clamped: the resting level is measured over a different stretch than the range, so noise can put it a
          // hair outside without meaning the hand rests beyond its own travel.
          math.max(0.0, math.min(1.0, (rest - lowest) / (highest - lowest)))
      .flatten

  /** How far after the exerciser's rep boundary this channel's peak falls, as a fraction of a rep.
    *
    * Zero when the hand rests where this channel is brightest, so its peak *is* the boundary; a half when the hand
    * rests where it is darkest, so its peak is the far end of the movement. Between the two it follows the phase of a
    * cosine at that height, which is the shape a limb sweeping through a quadrant traces.
    */
  def peakOffset(restFraction: Double): Double =
    math.acos(2 * math.max(0.0, math.min(1.0, restFraction)) - 1) / (2 * math.Pi)

  /** The offset for one channel of a recording, or `None` when the recording cannot say. */
  def offsetOf(channels: Map[Quadrant, Seq[Double]], quadrant: Quadrant): Option[Double] =
    for
      samples <- channels.get(quadrant)
      fraction <- restFraction(samples, stillPrefix(channels))
    yield peakOffset(fraction)
