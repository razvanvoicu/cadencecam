package sgrv.fe

import munit.FunSuite
import sgrv.fe.ApiClient.MeResult

class ApiClientSuite extends FunSuite:
  test("parses the signed-in user and the optional counting-session identity"):
    val json =
      """{"email":"developer@example.com","name":"Developer","extra":{"counting-session":{"sessionId":"a3f1"}}}"""

    assertEquals(
      ApiClient.parseUser(json),
      MeResult(UserState.SignedIn("developer@example.com", "Developer"), Some("a3f1"))
    )

  test("uses the email as the display name when the backend supplies no usable name"):
    val json = """{"email":"developer@example.com","name":"  "}"""

    assertEquals(
      ApiClient.parseUser(json),
      MeResult(UserState.SignedIn("developer@example.com", "developer@example.com"), None)
    )

  test("reports malformed user JSON as an authentication failure"):
    val result = ApiClient.parseUser("""{"email":12}""")

    assert(result.user.isInstanceOf[UserState.AuthenticationFailed])
    assertEquals(result.countingSessionId, None)

  test("does not turn an unreadable optional counting-session contribution into an authentication failure"):
    val json =
      """{"email":"developer@example.com","name":"Developer","extra":{"counting-session":{"wrong":"shape"}}}"""

    assertEquals(
      ApiClient.parseUser(json),
      MeResult(UserState.SignedIn("developer@example.com", "Developer"), None)
    )
