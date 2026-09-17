package sgrv.fe

import sgrv.api.{AccountSettings, CountsBy}

/** The arithmetic behind the dashboard: energy from movement, pace from a clock, and where both are heading.
  *
  * Separate from the view and free of any browser type, because these are the figures the whole screen exists to show
  * and they are worth being able to check without a camera, a link and a tablet.
  */
private[fe] object Effort:

  /** The projection always looks ahead to a round half hour, and never to one already behind.
    *
    * A fixed thirty-minute horizon stops meaning anything the moment a workout runs past it: a figure captioned "at 30
    * min" while the clock says thirty-four is not a projection at all, it is the present tense with a wrong label. So
    * the horizon moves on with the workout -- half an hour, an hour, an hour and a half -- and always stands ahead of
    * where the exerciser actually is.
    */
  val BoundaryStepMinutes = 30

  /** How long a set must have run before it is extrapolated at all.
    *
    * A projection is a multiplication by `boundary / elapsed`, and at four seconds in that is a multiplication by four
    * hundred: the figure would be a restatement of the first two reps with two orders of magnitude of confidence
    * attached. Half a minute is the point at which the number it produces is about the exercise rather than about its
    * first gap.
    */
  val MinimumProjectionSeconds = 30.0

  /** The next round boundary the workout has not yet reached, in minutes.
    *
    * Strictly ahead: at exactly thirty minutes the horizon is already the hour, because the thirty-minute figure has
    * stopped being a projection and become a total.
    */
  def boundaryMinutes(elapsedMinutes: Double): Int =
    if !elapsedMinutes.isFinite || elapsedMinutes < 0 then BoundaryStepMinutes
    else
      val steps = math.floor(elapsedMinutes / BoundaryStepMinutes) + 1
      // A workout of a thousand hours is a clock that has gone wrong rather than an exercise, and capping keeps the
      // arithmetic in range instead of overflowing into a caption nobody can read.
      BoundaryStepMinutes * math.min(steps, 2000.0).toInt

  /** How a boundary is said: half hours in minutes up to the hour, and in hours after it. */
  def boundaryLabel(minutes: Int): String =
    if minutes < 60 then s"$minutes min"
    else
      val hours = minutes / 60.0
      if hours == math.floor(hours) then s"${hours.toInt} h" else f"$hours%.1f h"

  /** A rate per minute, or nothing while there is no clock to divide by. */
  def perMinute(total: Double, elapsedMinutes: Double): Option[Double] =
    Option.when(elapsedMinutes > 0 && total.isFinite)(total / elapsedMinutes)

  /** What the total reaches at the next boundary if the pace so far is kept.
    *
    * Proportional to the average rather than built from the pace of the last few reps: those two are the same
    * arithmetic when the rate used is the average, and using the recent pace instead would make the projection lurch
    * by hundreds every time someone paused for breath.
    */
  def atBoundary(total: Double, elapsedMinutes: Double): Option[Double] =
    Option.when(elapsedMinutes * 60.0 >= MinimumProjectionSeconds && total.isFinite):
      total * boundaryMinutes(elapsedMinutes) / elapsedMinutes

  /** Energy, by whichever of the two measures the chosen exercise is counted by.
    *
    * Both are the account's own formula applied literally, weight in kilograms and the exercise's factor as set. What
    * they produce is not a calorie in any standard sense until the factor has been calibrated against something, which
    * is the whole reason the factor is a setting and starts at one.
    */
  def calories(reps: Int, cadenceSum: Double, settings: AccountSettings): Double =
    val measure = settings.countsBy match
      case CountsBy.RepCount  => reps.toDouble
      case CountsBy.Frequency => cadenceSum
    if !measure.isFinite then 0.0 else measure * settings.weightKilograms * settings.factor

  /** A whole number with its thousands grouped, which is what stops "1 236" reading as a stray digit beside a total. */
  def grouped(value: Double): String =
    val rounded = math.round(value)
    val digits = math.abs(rounded).toString
    val grouped = digits.reverse.grouped(3).mkString(",").reverse
    if rounded < 0 then s"-$grouped" else grouped

  /** A rate, to one decimal: it moves with every rep, and a second decimal would only ever be watched changing. */
  def rate(value: Double): String = f"$value%.1f"

  /** What a figure reads as before there is anything to say. An em dash rather than a zero, which would be a claim. */
  val Absent = "—"

  def elapsedClock(seconds: Double): String =
    val whole = math.max(0L, math.round(seconds))
    val minutes = whole / 60
    val remainder = whole % 60
    if minutes < 60 then f"$minutes%d:$remainder%02d"
    else f"${minutes / 60}%d:${minutes % 60}%02d:$remainder%02d"
