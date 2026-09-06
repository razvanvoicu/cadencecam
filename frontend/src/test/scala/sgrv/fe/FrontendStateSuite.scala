package sgrv.fe

import munit.FunSuite
import sgrv.api.AboutInfo
import zio.json.*

class FrontendStateSuite extends FunSuite:

  test("round-trips the complete frontend state through its browser-storage representation"):
    val original = FrontendState(
      user = UserState.SignedIn("developer@example.com", "Developer"),
      sheetName = "Sample",
      sheetState = SheetState.Loaded(Seq(Seq("timestamp", "browser"))),
      aboutState = AboutState.Loaded(AboutInfo("1.0", "today", "Mac", "3", "1")),
      logoutState = LogoutState.Failed("try again")
    )

    assertEquals(original.toJson.fromJson[FrontendState], Right(original))

  test("normalizes browser resources and authentication before startup validation"):
    val persisted = FrontendState.Initial.copy(
      user = UserState.SignedIn("developer@example.com", "Developer"),
      sheetName = "Remembered",
      sheetState = SheetState.Loading,
      aboutState = AboutState.Loading,
      logoutState = LogoutState.InProgress
    )

    assertEquals(
      persisted.prepareForStartup,
      FrontendState.Initial.copy(sheetName = "Remembered")
    )
