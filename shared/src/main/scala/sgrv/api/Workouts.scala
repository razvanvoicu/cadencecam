package sgrv.api

import zio.json.{DeriveJsonCodec, JsonCodec, jsonNoExtraFields}

/** The settings and conclusion belonging to one workout at the time it was performed. Settings can change later; a
  * history row must not silently rewrite its exercise, factor, or calories when they do.
  */
@jsonNoExtraFields
final case class WorkoutSnapshot(
    exerciseType: String,
    calories: Double,
    exerciseFactor: Double,
    weightKilograms: Double,
    countsBy: CountsBy
)

object WorkoutSnapshot:
  given JsonCodec[WorkoutSnapshot] = DeriveJsonCodec.gen[WorkoutSnapshot]

/** One workout, as the history lists it. The raw measurement remains beside the snapshot, so exports are auditable and
  * an older workout that predates snapshots can still be read without inventing values for it.
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
    endedBy: Option[String] = None,
    snapshot: Option[WorkoutSnapshot] = None
):
  /** Whether this is the workout in progress.
    *
    * Listed and marked rather than hidden: it is the one the dashboard's own figures belong to, and leaving it off
    * would read as the app having lost it. It can be deleted like any other -- the account's pointer to it goes with
    * it, and the device counting into it stands down at its next report.
    */
  def running: Boolean = endedAtMillis.isEmpty

object Workout:
  given JsonCodec[Workout] = DeriveJsonCodec.gen[Workout]

/** The deliberate lifecycle control for a workout. Login, logout and choosing the counter role do not imply either
  * action: a workout begins and ends only at the two controls bearing these names.
  */
enum WorkoutAction:
  case Start, Stop

object WorkoutAction:
  given JsonCodec[WorkoutAction] = DeriveJsonCodec.gen[WorkoutAction]

/** A workout lifecycle request. Both actions carry the settings snapshot; Stop also carries the final measurement so
  * closing a session cannot race its last periodic progress report and leave the history a few reps behind the screen.
  */
@jsonNoExtraFields
final case class WorkoutControl(
    action: WorkoutAction,
    progress: Option[RepProgress] = None,
    snapshot: Option[WorkoutSnapshot] = None
)

object WorkoutControl:
  val Path = "/countingSession/control"
  given JsonCodec[WorkoutControl] = DeriveJsonCodec.gen[WorkoutControl]

/** Whether the account's counter currently has a workout open. */
@jsonNoExtraFields
final case class WorkoutState(active: Boolean)

object WorkoutState:
  given JsonCodec[WorkoutState] = DeriveJsonCodec.gen[WorkoutState]

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
