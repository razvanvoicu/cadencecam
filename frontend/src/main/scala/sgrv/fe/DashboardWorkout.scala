package sgrv.fe

import sgrv.api.{AccountSettings, LiveReading, RepProgress, WorkoutAction, WorkoutControl, WorkoutSnapshot}

/** Builds the completed workout owned by the dashboard whose Stop control was pressed.
  *
  * The selected exercise id is resolved against this dashboard's settings, not against settings held by the counter or
  * by another dashboard. A real selection which is absent locally produces no command; silently falling back to the
  * generic factor would save a plausible but incorrect history row. The generic basis is used only when no exercise was
  * selected at all.
  */
private[fe] object DashboardWorkout:

  /** This dashboard's settings with the relayed selection applied. */
  def selectedSettings(settings: AccountSettings, exerciseId: Option[String]): Option[AccountSettings] =
    exerciseId match
      case None => Some(settings.copy(selected = None))
      case Some(id) if settings.exercises.exists(_.id == id) => Some(settings.copy(selected = Some(id)))
      case Some(_)                                           => None

  def progress(reading: LiveReading): RepProgress =
    RepProgress(reading.reps, reading.cadenceSum, reading.elapsedSeconds)

  /** The exact Stop request this dashboard will submit to history. */
  def stop(
      reading: LiveReading,
      settings: AccountSettings,
      exerciseId: Option[String]
  ): Option[WorkoutControl] =
    selectedSettings(settings, exerciseId).map: selected =>
      val measured = progress(reading)
      val exerciseType = selected.selectedExercise.map(_.name.trim).filter(_.nonEmpty).getOrElse("Counting")
      val snapshot = WorkoutSnapshot(
        exerciseType = exerciseType,
        calories = Effort.calories(measured.reps, measured.cadenceSum, selected),
        exerciseFactor = selected.factor,
        weightKilograms = selected.weightKilograms,
        countsBy = selected.countsBy
      )
      WorkoutControl(WorkoutAction.Stop, progress = Some(measured), snapshot = Some(snapshot))
