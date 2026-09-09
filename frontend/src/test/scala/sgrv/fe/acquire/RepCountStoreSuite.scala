package sgrv.fe.acquire

import munit.FunSuite
import zio.json.*

class RepCountStoreSuite extends FunSuite:

  private val hour = RepCountStore.DefaultRetentionMillis
  private val now = 1_700_000_000_000.0

  private def resumable(saved: Option[SavedRepCount], at: Double = now, retention: Double = hour): Int =
    RepCountStore.resumable(saved, at, retention)

  test("round-trips a saved reading through its browser-storage representation"):
    val original = SavedRepCount(57, now)

    assertEquals(original.toJson.fromJson[SavedRepCount], Right(original))

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
