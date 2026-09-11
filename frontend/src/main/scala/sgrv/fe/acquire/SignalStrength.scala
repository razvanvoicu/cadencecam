package sgrv.fe.acquire

/** How far the movement stands above the floor it has to clear, as a multiple of that floor.
  *
  * Bands set from measurement rather than taste. Across four phones running a movement of known cadence, a circling
  * disc scored between 3.4 and 4.7 and counted every rep; a swinging bar scored between 1.4 and 2.0, lost reps, and
  * took most of a minute to find the cadence at all. The boundary between those is where counting degrades.
  */
private[fe] enum SignalStrength:
  case Strong
  case Adequate
  case Weak

private[fe] object SignalStrength:
  val StrongAbove = 2.5
  val AdequateAbove = 1.5

  def of(margin: Double): SignalStrength =
    if margin >= StrongAbove then Strong
    else if margin >= AdequateAbove then Adequate
    else Weak

  /** What to show: the margin to one decimal, which is a multiple of the bar the movement must clear. */
  def label(margin: Double): String = f"$margin%.1f×"

  /** Deliberately not called confidence or quality.
    *
    * It says whether the movement is strong enough to be counted, and nothing about whether what is being counted is
    * exercise. On two of four phones, a camera shaken by typing on the same desk scored above four here while
    * counting nothing real -- as high as a movement that counted perfectly. Telling those apart needs the quadrants
    * to be compared with each other, which this does not do, so a word implying trustworthiness would mislead
    * exactly when it mattered most.
    */
  val Description = "how far the movement stands above the background"
