package sgrv.fe.acquire

/** How much the picture moves when nothing in it does.
  *
  * Reported because it decides whether counting is possible at all, and because the remedy is something only the person
  * holding the camera can apply: move back, add light, put the phone on something solid. A count that drifts upward
  * while a room is empty looks like a fault in the app, and on a noisy camera it is not one.
  *
  * Estimated as the quietest quadrant's own amplitude. Whatever is moving is rarely in all four at once, so the
  * smallest of them is the closest thing to a measurement of the background alone.
  */
private[fe] enum NoiseLevel:
  case Quiet
  case Fair
  case Noisy

private[fe] object NoiseLevel:

  /** Where noise stops being harmless and starts setting the bar itself.
    *
    * Not a taste: a peak must clear `max(prominenceFloor, prominenceFactor * amplitude)`, so while the noise stays
    * below `prominenceFloor / prominenceFactor` the fixed floor is what a movement has to beat, and the noise costs
    * nothing. Above it the noise sets its own bar -- and a bar set by noise is one that noise can clear, since peaks
    * routinely exceed one and a half times their own RMS. That is the point at which a camera becomes unusable, and it
    * is worth telling someone before they run a set rather than after.
    */
  def governs(settings: DetectorSettings): Double = settings.prominenceFloor / settings.prominenceFactor

  def of(noise: Double, settings: DetectorSettings = DetectorSettings()): NoiseLevel =
    val takesOver = governs(settings)
    if noise >= takesOver then Noisy
    else if noise >= takesOver / 2 then Fair
    else Quiet

  /** The amplitude itself, to one decimal: the same units the prominence floor is in, so the two can be compared. */
  def label(noise: Double): String = f"$noise%.1f"

  val Description = "how much the picture moves when nothing does"
