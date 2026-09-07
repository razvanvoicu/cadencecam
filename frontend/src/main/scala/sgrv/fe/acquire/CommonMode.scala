package sgrv.fe.acquire

/** Removes what every quadrant is doing at once, leaving what they are doing differently.
  *
  * A camera re-converging its exposure or white balance shifts every pixel in the same direction at the same instant,
  * so it survives the averaging inside a quadrant untouched and lands in all four channels identically. A step like
  * that is broadband: pushed through the band-pass it rings, and one cycle of ringing in the passband is
  * indistinguishable from a rep.
  *
  * Subtracting the across-quadrant mean at each instant cancels that exactly, because it is common to all four. Real
  * movement is differential and largely survives: an object crossing some quadrants and not others, or reaching them at
  * different times as a rotation does, leaves the instantaneous mean roughly unchanged while the individual channels
  * swing. The one thing this would cancel is a movement filling all four quadrants in phase and in equal measure, which
  * is the case the acquisition design already assumes does not arise.
  */
private[fe] object CommonMode:
  /** Subtracts, from each channel, the mean across channels at the same instant.
    *
    * Channels are truncated to the shortest, so a window read mid-sample cannot misalign them — subtracting values from
    * different instants would inject noise rather than remove it.
    */
  def remove(byQuadrant: Map[Quadrant, Seq[Double]]): Map[Quadrant, Seq[Double]] =
    if byQuadrant.isEmpty then byQuadrant
    else
      val length = byQuadrant.values.map(_.size).min
      val channels = byQuadrant.view.mapValues(_.takeRight(length)).toMap
      val means = Seq.tabulate(length): index =>
        channels.values.map(_(index)).sum / channels.size
      channels.view.mapValues(_.zip(means).map((value, mean) => value - mean)).toMap
