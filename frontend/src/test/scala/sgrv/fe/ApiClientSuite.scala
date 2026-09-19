package sgrv.fe

import munit.FunSuite
import sgrv.fe.ApiClient.{ApiError, MeResult, RetryPolicy}

class ApiClientSuite extends FunSuite:
  test("parses the signed-in user and the optional counting-session identity"):
    val json =
      """{"email":"developer@example.com","name":"Developer","extra":{"counting-session":{"sessionId":"a3f1"}}}"""

    assertEquals(
      ApiClient.parseUser(json),
      Right(MeResult(UserState.SignedIn("developer@example.com", "Developer"), Some("a3f1")))
    )

  test("uses the email as the display name when the backend supplies no usable name"):
    val json = """{"email":"developer@example.com","name":"  "}"""

    assertEquals(
      ApiClient.parseUser(json),
      Right(MeResult(UserState.SignedIn("developer@example.com", "developer@example.com"), None))
    )

  test("reports malformed user JSON as an authentication failure"):
    val result = ApiClient.parseUser("""{"email":12}""")

    assert(result.isLeft)
    assert(result.left.exists(_.isInstanceOf[ApiError.Decode]))

  test("does not turn an unreadable optional counting-session contribution into an authentication failure"):
    val json =
      """{"email":"developer@example.com","name":"Developer","extra":{"counting-session":{"wrong":"shape"}}}"""

    assertEquals(
      ApiClient.parseUser(json),
      Right(MeResult(UserState.SignedIn("developer@example.com", "Developer"), None))
    )

  test("an HTTP error includes a useful bounded response body"):
    val longBody = "x" * 500
    val retained = ApiClient.errorBody(s"  $longBody  ")

    assertEquals(retained.map(_.length), Some(401))
    assert(retained.exists(_.endsWith("…")))
    assertEquals(ApiClient.errorBody("  \n "), None)
    assertEquals(
      ApiError.Http("save account settings", 409, Some("conflict")).message,
      "Could not save account settings (HTTP 409): conflict"
    )

  test("only temporary read failures are retried, and only once"):
    val reads = RetryPolicy.IdempotentRead

    assert(reads.shouldRetryStatus(503, attemptsAlreadyMade = 0))
    assert(reads.shouldRetryStatus(429, attemptsAlreadyMade = 0))
    assert(!reads.shouldRetryStatus(500, attemptsAlreadyMade = 0))
    assert(!reads.shouldRetryStatus(503, attemptsAlreadyMade = 1))
    assert(reads.shouldRetryTransport(attemptsAlreadyMade = 0))
    assert(!reads.shouldRetryTransport(attemptsAlreadyMade = 1))
    assert(!RetryPolicy.Never.shouldRetryTransport(attemptsAlreadyMade = 0))
