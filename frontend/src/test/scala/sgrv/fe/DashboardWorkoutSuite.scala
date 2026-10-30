package sgrv.fe

import munit.FunSuite
import sgrv.api.{AccountSettings, CountsBy, ExerciseType, LiveReading, Workout}

class DashboardWorkoutSuite extends FunSuite:

  private val exerciseId = "cycle"

  private def dashboardSettings(factor: Double): AccountSettings =
    AccountSettings(
      weightKilograms = 80.0,
      defaultFactor = 0.5,
      exercises = Seq(ExerciseType(exerciseId, "Stationary bicycle", None, CountsBy.Frequency, factor)),
      selected = Some(exerciseId)
    )

  test("the dashboard where Stop is pressed supplies its factor and calories to history"):
    val reading = LiveReading(
      reps = 100,
      repsPerMinute = 40.0,
      status = "Counting",
      active = true,
      elapsedSeconds = 150.0,
      cadenceSum = 40.0
    )
    val dashboardA = dashboardSettings(1.25)
    val dashboardB = dashboardSettings(1.80)

    val aStop = DashboardWorkout.stop(reading, dashboardA, Some(exerciseId)).get
    val bStop = DashboardWorkout.stop(reading, dashboardB, Some(exerciseId)).get
    // Simulates the backend creating the history row from the request made by Dashboard B, where Stop was pressed.
    val historyRecord = Workout(
      id = "w1",
      startedAtMillis = 1_000.0,
      endedAtMillis = Some(2_000.0),
      reps = bStop.progress.get.reps,
      cadenceSum = bStop.progress.get.cadenceSum,
      elapsedSeconds = bStop.progress.get.elapsedSeconds,
      snapshot = bStop.snapshot
    )
    val saved = historyRecord.snapshot.get

    assertEqualsDouble(saved.exerciseFactor, 1.80, 1e-9)
    assertEqualsDouble(saved.calories, 40.0 * 80.0 * 1.80 / 100.0, 1e-9)
    assertNotEquals(saved.exerciseFactor, aStop.snapshot.get.exerciseFactor)
    assertNotEquals(saved.calories, aStop.snapshot.get.calories)
    assertNotEquals(saved.exerciseFactor, dashboardB.defaultFactor)
