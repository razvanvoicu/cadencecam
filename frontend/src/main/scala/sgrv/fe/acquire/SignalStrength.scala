package sgrv.fe.acquire

/** How far the movement stands above the floor it has to clear, as a multiple of that floor.
  *
  * Bands set from measurement rather than taste, and re-set once the measurement got large enough to trust. The first
  * pair came from eight readings on four phones; these come from replaying the detector over 452 recorded bench tests
  * of a hundred reps each, and they moved a long way.
  *
  * What the 452 say: above a margin of two, a test lands within two reps of the truth 93% of the time and exactly on it
  * 71% of the time, and neither figure improves with a stronger signal -- the rate is flat from two all the way to
  * fifteen. Below one and a half, it falls to 57% and 29%. Between them sits a thin band of sixteen recordings at 75%.
  *
  * The old boundary for "strong" was 3.13, which separated almost nothing: the tests below it landed within two 86% of
  * the time against 94% above, so most of what it painted amber counted perfectly well. That is what this fixes.
  */
private[fe] enum SignalStrength:
  case Strong
  case Adequate
  case Weak

private[fe] object SignalStrength:
  /** The bands, as multiples of the floor.
    *
    * Stated in prominence so the boundaries stay where they were measured. The margin divides by the detector's
    * prominence floor, so lowering that floor raises every margin by the same proportion -- and a badge that quietly
    * re-graded every movement each time a detector threshold moved would be reporting the threshold, not the signal.
    * These are the measured boundaries of 8.0 and 6.0 in prominence, expressed against whatever the floor currently is.
    *
    * The lower one rests on much less evidence than the upper: 23 of the 452 recordings sat below a margin of two and
    * only 7 below one and a half, so where exactly counting stops is known far less precisely than where it starts.
    */
  private val StrongProminence = 8.0
  private val AdequateProminence = 6.0

  val StrongAbove: Double = StrongProminence / DetectorSettings().prominenceFloor
  val AdequateAbove: Double = AdequateProminence / DetectorSettings().prominenceFloor

  def of(margin: Double): SignalStrength =
    if margin >= StrongAbove then Strong
    else if margin >= AdequateAbove then Adequate
    else Weak

  /** What to show: the margin to one decimal, as a multiple of the prominence floor. */
  def label(margin: Double): String = f"$margin%.1f×"

  /** Deliberately not called confidence or quality.
    *
    * It says whether the movement is strong enough to be counted, and nothing about whether what is being counted is
    * exercise. On two of four phones, a camera shaken by typing on the same desk scored above four here while counting
    * nothing real -- as high as a movement that counted perfectly. Telling those apart needs the quadrants to be
    * compared with each other, which this does not do, so a word implying trustworthiness would mislead exactly when it
    * mattered most.
    */
  val Description = "how far the movement stands above the background"
