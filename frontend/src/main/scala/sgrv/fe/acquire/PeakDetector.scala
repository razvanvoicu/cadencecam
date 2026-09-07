package sgrv.fe.acquire

/** Finds the local maxima that count as reps, on an already band-passed series.
  *
  * Real signals here are not clean sinusoids: a cycle can carry notches and shoulders that are local maxima without
  * being reps. Two constraints separate them, the same pair used for pulse detection from PPG, which sits in a
  * similar frequency range and has the same problem:
  *
  *   - **prominence**, how far a peak stands above the higher of the two valleys flanking it. More robust than an
  *     absolute height threshold, which drifts with the baseline.
  *   - **distance**, a floor on the interval between accepted peaks, taken from the fastest cadence considered
  *     physically possible.
  *
  * A peak is only confirmed once the signal has turned back down after it, so the newest sample can never be one.
  * That is a delay of a single sample, not of a window, which is why counting works from peaks directly rather than
  * from a windowed frequency estimate.
  */
private[fe] object PeakDetector:

  /** How far a local maximum stands above the higher of the valleys either side of it.
    *
    * Each side is walked outwards until the series rises to meet the peak again or the data runs out, and the
    * lowest point reached on the way is that side's valley.
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
    val candidates = localMaxima(samples)
      .map(index => index -> prominence(samples, index))
      .filter((_, prominence) => prominence >= minimumProminence)
      .sortBy((index, prominence) => (-prominence, index))

    val accepted = candidates.foldLeft(Vector.empty[Int]): (kept, candidate) =>
      val (index, _) = candidate
      if kept.exists(other => math.abs(other - index) < minimumDistance) then kept else kept :+ index
    accepted.sorted
