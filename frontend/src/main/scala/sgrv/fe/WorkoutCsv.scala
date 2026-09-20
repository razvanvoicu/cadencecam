package sgrv.fe

import sgrv.api.Workout

/** The account's history as a file it can keep.
  *
  * A copy somebody can open in a spreadsheet and read without this app, which is the point: data you can look at only
  * through the thing that collected it is not really yours. So the columns are the measurement and not the app's
  * internals, and the calorie figure is written out beside what it was computed from -- a reader who later changes
  * their factor can redo the arithmetic, which they could not do from the total alone.
  */
private[fe] object WorkoutCsv:

  val FileName = "cadencecam-workouts.csv"

  val Header: Seq[String] =
    Seq(
      "started",
      "ended",
      "exercise_type",
      "duration_seconds",
      "reps",
      "cadence_sum",
      "weight_kg",
      "factor",
      "counts_by",
      "calories"
    )

  /** The whole history as one CSV document.
    *
    * `at` formats an instant, and is passed in rather than taken from the browser so this can be checked without one.
    */
  def of(workouts: Seq[Workout], at: Double => String): String =
    val rows = workouts.map: workout =>
      val saved = workout.snapshot
      Seq(
        at(workout.startedAtMillis),
        workout.endedAtMillis.fold("")(at),
        saved.fold("")(_.exerciseType),
        number(workout.elapsedSeconds, 1),
        workout.reps.toString,
        number(workout.cadenceSum, 3),
        saved.fold("")(value => number(value.weightKilograms, 1)),
        saved.fold("")(value => number(value.exerciseFactor, 2)),
        saved.fold("")(_.countsBy.toString),
        saved.fold("")(value => number(value.calories, 1))
      )
    (Header +: rows).map(_.map(escape).mkString(",")).mkString("\r\n") + "\r\n"

  /** A field as a CSV field: quoted when it holds anything that would otherwise end it early.
    *
    * The rule is RFC 4180's. None of this app's own values need it today -- they are dates and numbers -- but an
    * exercise is named by the person doing the exercising, and one called `Squats, heavy` would otherwise shift every
    * column after it by one and quietly corrupt the file.
    */
  private[fe] def escape(field: String): String =
    if field.exists(character => character == ',' || character == '"' || character == '\n' || character == '\r') then
      "\"" + field.replace("\"", "\"\"") + "\""
    else field

  /** A number with a fixed number of decimals, and never in exponent form: a spreadsheet reads `1.3E-4` as text. */
  private[fe] def number(value: Double, decimals: Int): String =
    if !value.isFinite then ""
    else
      val scale = math.pow(10, decimals)
      val rounded = math.round(value * scale) / scale
      s"%.${decimals}f".format(rounded)
