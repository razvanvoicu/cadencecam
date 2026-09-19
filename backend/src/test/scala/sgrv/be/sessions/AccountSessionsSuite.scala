package sgrv.be.sessions

import java.time.{Duration, Instant}

class AccountSessionsSuite extends munit.FunSuite:

  private val now = Instant.parse("2026-09-16T12:00:00Z")
  private val tenMinutes = AccountSessions.defaultIdle

  test("the timeout is ten minutes unless it is configured otherwise"):
    assertEquals(AccountSessions.defaultIdle, Duration.ofMinutes(10))

  test("a session that has counted recently stays open"):
    assert(!AccountSessions.goneQuiet(Some(now.minusSeconds(9 * 60)), now, tenMinutes))

  test("a session that has not counted for longer than the timeout has gone quiet"):
    assert(AccountSessions.goneQuiet(Some(now.minusSeconds(11 * 60)), now, tenMinutes))

  test("a session whose last mark is long past has gone quiet, however long ago it opened"):
    // The mark is moved by every report the counter makes, so this is a counter that stopped reporting half an hour
    // ago: its page closed, or its phone asleep.
    val lastReport = Some(now.minusSeconds(30 * 60))

    assert(AccountSessions.goneQuiet(lastReport, now, tenMinutes))

  test("a session that has never counted is not closed for being quiet"):
    // It has just opened; the operator is still framing the camera.
    assert(!AccountSessions.goneQuiet(None, now, tenMinutes))

  test("a session is kept as a record, and says why it ended"):
    assertEquals(SessionEnd.values.map(_.toString).toSet, Set("LoggedOut", "Idle", "TakenOver"))
