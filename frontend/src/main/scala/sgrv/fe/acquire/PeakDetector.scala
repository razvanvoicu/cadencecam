package sgrv.fe.acquire

/** Finds the local maxima that count as reps, on an already band-passed series.
  *
  * Real signals here are not clean sinusoids: a cycle can carry notches and shoulders that are local maxima without
  * being reps. Two constraints separate them, the same pair used for pulse detection from PPG, which sits in a similar
  * frequency range and has the same problem:
  *
  *   - **prominence**, how far a peak stands above the higher of the two valleys flanking it. More robust than an
  *     absolute height threshold, which drifts with the baseline.
  *   - **distance**, a floor on the interval between accepted peaks, taken from the fastest cadence considered
  *     physically possible.
  *
  * A peak is only confirmed once the signal has turned back down after it, so the newest sample can never be one. That
  * is a delay of a single sample, not of a window, which is why counting works from peaks directly rather than from a
  * windowed frequency estimate.
  */
private[fe] object PeakDetector:

  /** How far a local maximum stands above the higher of the valleys either side of it.
    *
    * Each side is walked outwards until the series rises to meet the peak again or the data runs out, and the lowest
    * point reached on the way is that side's valley.
    */
  private[acquire] def prominence(samples: Seq[Double], index: Int): Double =
    val height = samples(index)

    def valley(steps: Iterator[Int]): Double =
      var lowest = height
      var stop = false
      while steps.hasNext && !stop do
        val value = samples(steps.next())
        if value >= height then stop = true
        else if value < lowest then lowest = value
      lowest

    val left = valley(Iterator.range(index - 1, -1, -1))
    val right = valley(Iterator.range(index + 1, samples.length))
    height - math.max(left, right)

  private[acquire] def localMaxima(samples: Seq[Double]): Seq[Int] =
    if samples.length < 3 then Seq.empty
    else (1 until samples.length - 1).filter(i => samples(i) > samples(i - 1) && samples(i) >= samples(i + 1))

  /** Indices of accepted peaks, in order.
    *
    * Where two candidates fall closer together than `minimumDistance`, the more prominent survives — resolving by
    * prominence rather than by position keeps the choice independent of which end the series is scanned from.
    */
  def peaks(samples: Seq[Double], minimumDistance: Int, minimumProminence: Double): Seq[Int] =
    measured(samples, minimumDistance, minimumProminence).map(_._1)

  /** Accepted peaks with how far each stood out, which the selection has already worked out.
    *
    * Returned rather than recomputed: how far a movement stands above the bar it has to clear is worth reporting to the
    * user, and asking for it again would mean walking the series once more for every peak.
    */
  def measured(
      samples: Seq[Double],
      minimumDistance: Int,
      minimumProminence: Double
  ): Seq[(Int, Double)] =
    val candidates = localMaxima(samples)
      .map(index => index -> prominence(samples, index))
      .filter((_, prominence) => prominence >= minimumProminence)
      .sortBy((index, prominence) => (-prominence, index))

    val accepted = candidates.foldLeft(Vector.empty[(Int, Double)]): (kept, candidate) =>
      val (index, _) = candidate
      if kept.exists((other, _) => math.abs(other - index) < minimumDistance) then kept else kept :+ candidate
    accepted.sortBy(_._1)

  /** How far the series falls below a peak within `within` samples after it.
    *
    * `None` when there is not yet that much signal after the peak: a peak at the edge of the buffer has not been judged
    * rather than judged badly, and the next sample or two will settle it.
    */
  private[acquire] def fallAfter(samples: Seq[Double], peak: Int, within: Int): Option[Double] =
    val last = peak + within
    Option.when(peak >= 0 && last < samples.length):
      samples(peak) - samples.slice(peak + 1, last + 1).min

  /** The peaks the movement came back from.
    *
    * A rep is a round trip: the object enters a quadrant and leaves it again, so the brightness that rose comes back
    * down within a period. Putting the weight down is not a round trip -- the object goes and stays gone -- and the
    * brightness holds at its new level. Both look identical to a band-pass, which is why the one was being counted as
    * the other: measured across twenty-four recorded tests, every dark-object-on-light-ground test ended with a phantom
    * rep at the moment the object left the frame.
    *
    * Told apart here on the quadrant's own brightness rather than on the filtered signal, because the filter erases the
    * distinction: it is blind to a constant, so a level that rose and stayed and a level that rose and fell both decay
    * back to zero in it.
    *
    * The scale is the movement's own. How far a rep falls depends on contrast, framing and distance, none of which are
    * known in advance; how far this peak fell against how far the others did is comparable across all of them. In those
    * recordings every real rep came back at least a third as far as the typical one, and every phantom less than a
    * seventh -- so the bar sits between, and nearer the phantoms.
    */
  private[acquire] def returning(
      samples: Seq[Double],
      peaks: Seq[Int],
      within: Int,
      fraction: Double
  ): Seq[Int] =
    val judged = peaks.flatMap(peak => fallAfter(samples, peak, within).map(peak -> _))
    if judged.isEmpty then Seq.empty
    else
      val falls = judged.map(_._2).sorted
      val middle = falls.length / 2
      val typical = if falls.length % 2 == 1 then falls(middle) else (falls(middle - 1) + falls(middle)) / 2
      judged.collect { case (peak, fall) if fall >= fraction * typical => peak }
