package sgrv.fe.refreshstate

import munit.FunSuite
import zio.json.*

class RefreshStateSuite extends FunSuite:

  test("round-trips refresh state without an application-state codec"):
    val original = RefreshState(active = true, generation = 7, nextAttemptAtMillis = Some(1234d), expired = true)

    assertEquals(original.toJson.fromJson[RefreshState], Right(original))

  test("invalidates persisted browser callbacks at startup"):
    val persisted = RefreshState(active = true, generation = 7, nextAttemptAtMillis = Some(1234d), expired = true)

    assertEquals(
      persisted.prepareForStartup,
      RefreshState(active = false, generation = 8, nextAttemptAtMillis = None, expired = false)
    )
