package sgrv.fe

import munit.FunSuite
import sgrv.api.AboutInfo
import zio.json.*

class FrontendStateSuite extends FunSuite:

  test("round-trips the complete frontend state through its browser-storage representation"):
    val original = FrontendState(
      user = UserState.SignedIn("developer@example.com", "Developer"),
      screen = Screen.Acquirer,
      countingSessionId = Some("a3f1"),
      aboutState = AboutState.Loaded(AboutInfo("1.0", "today", "Mac", "3", "1")),
      logoutState = LogoutState.Failed("try again")
    )

    assertEquals(original.toJson.fromJson[FrontendState], Right(original))

  test("normalizes browser resources and authentication before startup validation"):
    val persisted = FrontendState.Initial.copy(
      user = UserState.SignedIn("developer@example.com", "Developer"),
      screen = Screen.Dashboard,
      countingSessionId = Some("a3f1"),
      aboutState = AboutState.Loading,
      logoutState = LogoutState.InProgress
    )

    assertEquals(
      persisted.prepareForStartup,
      FrontendState.Initial.copy(
        user = UserState.Restoring("developer@example.com", "Developer"),
        screen = Screen.Dashboard,
        countingSessionId = Some("a3f1")
      )
    )

  test("only a previously signed-in user is restored optimistically"):
    def startupUser(user: UserState): UserState = FrontendState.Initial.copy(user = user).prepareForStartup.user

    assertEquals(startupUser(UserState.Unauthenticated), UserState.Unknown)
    assertEquals(startupUser(UserState.AuthenticationFailed("nope")), UserState.Unknown)
    assertEquals(startupUser(UserState.Unknown), UserState.Unknown)
    // Restoring is idempotent, so a reload during an unverified restore does not lose the view.
    assertEquals(
      startupUser(UserState.Restoring("a@b.c", "A")),
      UserState.Restoring("a@b.c", "A")
    )

  test("the shell renders for both confirmed and restoring users, and for nobody else"):
    assertEquals(Present.unapply(UserState.SignedIn("a@b.c", "Ada")), Some("Ada"))
    assertEquals(Present.unapply(UserState.Restoring("a@b.c", "Ada")), Some("Ada"))
    assertEquals(Present.unapply(UserState.Unauthenticated), None)
    assertEquals(Present.unapply(UserState.Unknown), None)
    assertEquals(Present.unapply(UserState.AuthenticationFailed("nope")), None)
