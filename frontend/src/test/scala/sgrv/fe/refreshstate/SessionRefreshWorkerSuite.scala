package sgrv.fe.refreshstate

import munit.FunSuite

class SessionRefreshWorkerSuite extends FunSuite:

  test("schedules renewal at the configured lead time before expiry"):
    val now = 1_000_000d
    val lifetime = 7 * 24 * 60 * 60 * 1000d
    val leadTime = 5 * 60 * 1000

    assertEquals(
      SessionRefreshWorker.nextDelayMillis(now, now + lifetime, leadTime),
      (lifetime - leadTime).toInt
    )

  test("schedules an imminent or overdue renewal without a tight loop"):
    assertEquals(SessionRefreshWorker.nextDelayMillis(10_000d, 10_000d, 5_000), 1000)
    assertEquals(SessionRefreshWorker.nextDelayMillis(10_000d, 14_000d, 5_000), 1000)

  test("caps browser timers at the largest supported integer delay"):
    assertEquals(
      SessionRefreshWorker.nextDelayMillis(0d, Int.MaxValue.toDouble * 2, 0),
      Int.MaxValue
    )
