package sgrv.api

import zio.json.{DeriveJsonCodec, JsonCodec, jsonNoExtraFields}

/** One workout, as the history lists it.
  *
  * Carries what was measured rather than what was concluded from it. Calories are not stored: they depend on a weight
  * and a factor that the account can change afterwards, and a figure computed under an old factor and kept would be a
  * number nothing on the screen agreed with. The measurement -- reps, and the cadence accumulated over them -- is what
  * does not change, so that is what is kept and the figures are worked out from it when they are shown.
  */
@jsonNoExtraFields
final case class Workout(
    id: String,
    startedAtMillis: Double,
    endedAtMillis: Option[Double] = None,
    reps: Int = 0,
    cadenceSum: Double = 0.0,
    /** How long the set ran, from its first counted rep. Zero for a session that never counted one. */
    elapsedSeconds: Double = 0.0,
    /** Why it ended, absent while it is still running. */
    endedBy: Option[String] = None
):
  /** Whether this is the workout in progress. The one still running is shown, and cannot be deleted from under itself. */
  def running: Boolean = endedAtMillis.isEmpty

object Workout:
  given JsonCodec[Workout] = DeriveJsonCodec.gen[Workout]

/** The account's workouts, newest first. */
@jsonNoExtraFields
final case class WorkoutHistory(workouts: Seq[Workout] = Seq.empty)

object WorkoutHistory:
  val Path = "/countingSession/history"

  /** How many are listed. Enough for months of daily exercise, and bounded so one request cannot ask for a database.
    */
  val Limit = 200

  given JsonCodec[WorkoutHistory] = DeriveJsonCodec.gen[WorkoutHistory]

/** One workout the account is throwing away. */
@jsonNoExtraFields
final case class DiscardWorkout(id: String)

object DiscardWorkout:
  val Path = "/countingSession/history/discard"
  given JsonCodec[DiscardWorkout] = DeriveJsonCodec.gen[DiscardWorkout]
