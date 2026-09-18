package sgrv.fe

import munit.FunSuite
import sgrv.api.{AccountSettings, CountsBy, ExerciseType, Workout}

class WorkoutCsvSuite extends FunSuite:

  private val settings = AccountSettings(
    weightKilograms = 98.0,
    defaultFactor = 1.25,
    exercises = Seq(ExerciseType("stairs", "Stair climber", None, CountsBy.Frequency, 1.25)),
    selected = Some("stairs")
  )

  private def at(millis: Double): String = s"t$millis%.0f".format(millis)

  private val workout =
    Workout("w1", 1000.0, Some(2000.0), reps = 1000, cadenceSum = 528.6, elapsedSeconds = 1890.0)

  private def rows(document: String) = document.split("\r\n").filter(_.nonEmpty).toSeq

  test("the header names the measurement and what the figure was computed from"):
    // A total on its own cannot be redone by a reader who later changes their factor. The weight, the factor and the
    // measure are written beside it so the arithmetic is theirs to repeat.
    assertEquals(rows(WorkoutCsv.of(Seq.empty, settings, at)).head.split(",").toSeq, WorkoutCsv.Header)

  test("a workout is one row, with its calories worked out from the settings given"):
    val row = rows(WorkoutCsv.of(Seq(workout), settings, at))(1).split(",").toSeq

    assertEquals(row(3), "1000")
    assertEquals(row(4), "528.600")
    assertEquals(row(5), "98.0")
    assertEquals(row(6), "1.25")
    assertEquals(row(7), "Frequency")
    // 528.6 x 98 x 1.25 / 100, the same arithmetic the dashboard shows.
    assertEquals(row(8), "647.5")

  test("a workout still running has no end, and says so with an empty field rather than a guess"):
    val row = rows(WorkoutCsv.of(Seq(workout.copy(endedAtMillis = None)), settings, at))(1)

    assertEquals(row.split(",", -1)(1), "")

  test("every line ends the way a CSV reader expects, including the last"):
    val document = WorkoutCsv.of(Seq(workout), settings, at)

    assert(document.endsWith("\r\n"), "the final row needs its ending too")
    assertEquals(document.split("\r\n").filter(_.nonEmpty).length, 2)

  test("a field that would end a row early is quoted instead"):
    // Nothing this app writes needs it today -- these are dates and numbers -- but an exercise is named by the person
    // doing it, and "Squats, heavy" would otherwise shift every column after it by one.
    assertEquals(WorkoutCsv.escape("Squats, heavy"), "\"Squats, heavy\"")
    assertEquals(WorkoutCsv.escape("the \"heavy\" one"), "\"the \"\"heavy\"\" one\"")
    assertEquals(WorkoutCsv.escape("two\nlines"), "\"two\nlines\"")
    assertEquals(WorkoutCsv.escape("Stair climber"), "Stair climber")

  test("numbers are written out, never in exponent form, which a spreadsheet reads as text"):
    assertEquals(WorkoutCsv.number(0.00013, 3), "0.000")
    assertEquals(WorkoutCsv.number(1234567.891, 1), "1234567.9")
    assertEquals(WorkoutCsv.number(Double.NaN, 1), "")
    assertEquals(WorkoutCsv.number(Double.PositiveInfinity, 1), "")

  test("the file is named after the app, so a downloads folder still says what it is a month later"):
    assert(WorkoutCsv.FileName.startsWith("cadencecam"))
    assert(WorkoutCsv.FileName.endsWith(".csv"))
