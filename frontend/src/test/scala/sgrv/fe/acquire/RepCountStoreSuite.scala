package sgrv.fe.acquire

import munit.FunSuite
import sgrv.api.RepProgress
import zio.json.*

class RepCountStoreSuite extends FunSuite:

  private val hour = RepCountStore.DefaultRetentionMillis
  private val now = 1_700_000_000_000.0

  private def resumable(saved: Option[SavedRepCount], at: Double = now, retention: Double = hour): Int =
    RepCountStore.resumable(saved, at, retention).count

  private def resumedRun(saved: Option[SavedRepCount], at: Double = now, retention: Double = hour): ResumedRun =
    RepCountStore.resumable(saved, at, retention)

  test("round-trips a saved reading through its browser-storage representation"):
    val original = SavedRepCount(57, now, cadenceSum = 31.5, elapsedSeconds = 184.0)

    assertEquals(original.toJson.fromJson[SavedRepCount], Right(original))

  test("a reload resumes the whole run, not only its reps"):
    // Reps, calories and the clock are one measurement of one set. Resuming the count while restarting the other two
    // would show a workout that had done two hundred reps in no time at all and burned nothing doing it.
    val saved = SavedRepCount(57, now - 1000, cadenceSum = 31.5, elapsedSeconds = 184.0)

    assertEquals(resumedRun(Some(saved)), ResumedRun(57, 31.5, 184.0))

  test("nothing worth resuming resumes nothing, rather than a clock with no reps against it"):
    assertEquals(resumedRun(None), ResumedRun.Nothing)
    assertEquals(resumedRun(Some(SavedRepCount(57, now - 24 * hour, 31.5, 184.0))), ResumedRun.Nothing)

  test("resumes a total saved within the retention window"):
    assertEquals(resumable(Some(SavedRepCount(57, now - 1000))), 57)
    assertEquals(resumable(Some(SavedRepCount(57, now - hour + 1))), 57)

  test("abandons a total older than the retention window"):
    assertEquals(resumable(Some(SavedRepCount(57, now - hour))), 0)
    assertEquals(resumable(Some(SavedRepCount(57, now - 24 * hour))), 0)

  test("starts from zero when nothing was saved"):
    assertEquals(resumable(None), 0)

  test("ignores a saved zero, which carries no work to resume"):
    assertEquals(resumable(Some(SavedRepCount(0, now))), 0)

  test("a clock nudged backwards does not cost the user the count"):
    // Time synchronisation correcting the device clock by a few seconds is ordinary, and the reading it makes appear
    // to come from the future is the one just written.
    assertEquals(resumable(Some(SavedRepCount(57, now + 5000))), 57)

  test("a clock wrong by more than the window does not preserve a total indefinitely"):
    assertEquals(resumable(Some(SavedRepCount(57, now + 24 * hour))), 0)

  test("the retention window is configurable, and an hour is only the default"):
    val saved = Some(SavedRepCount(57, now - 90 * 1000))

    assertEquals(resumable(saved, retention = 60 * 1000.0), 0)
    assertEquals(resumable(saved, retention = 120 * 1000.0), 57)
    assertEquals(RepCountStore.DefaultRetentionMillis, 3_600_000.0)

  test("a carried-over run and what the detector has counted since add up to the whole workout"):
    // A reload or a camera switch leaves a run behind, and the detector starts again from nothing. All three figures
    // join together, or the history is told a workout of two hundred reps that took no time at all.
    val carried = ResumedRun(57, 31.5, 184.0)
    val since = RepReading(12, LockState.Searching, cadenceSum = 6.25, elapsedSeconds = 30.0)

    assertEquals(carried.plus(since), ResumedRun(69, 37.75, 214.0))
    assertEquals(ResumedRun.Nothing.plus(since), ResumedRun(12, 6.25, 30.0))

  test("the account's session is told the whole measurement, not only the count"):
    // The history works a duration and a calorie figure out of what the session was told. Told only the count, it
    // showed every workout as having taken no time and, counted by frequency, burned nothing.
    assertEquals(ResumedRun(69, 37.75, 214.0).progress, RepProgress(69, 37.75, 214.0))
