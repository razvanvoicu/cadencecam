package sgrv.fe

import munit.FunSuite
import sgrv.api.Documents

class MenuSuite extends FunSuite:

  test("every menu offers both documents, in the order they are published"):
    // The reason these are data rather than markup in three places: a privacy policy reachable from three screens and
    // not from the fourth is a gap nobody notices until somebody goes looking for it, and by then they are suspicious.
    assertEquals(Menu.documents.map(_._1), Seq("Privacy", "Terms of Service"))
    assertEquals(Menu.documents.map(_._2), Seq("/privacy.html", "/tos.html"))

  test("the links ask for exactly the paths the backend serves"):
    // Both ends read one list. A link that four-oh-fours is worse than no link at all on this particular page.
    assertEquals(Menu.documents.map(_._2), Documents.All.map(Documents.path))

  test("every published document has a name to show, so none can be added without one"):
    assertEquals(Documents.All.filterNot(Menu.documentLabels.contains), Seq.empty)

  test("a document's path is its file, at the root"):
    assertEquals(Documents.path(Documents.Privacy), "/privacy.html")
    assertEquals(Documents.path(Documents.Terms), "/tos.html")

  test("a menu is placed below and right-aligned with the button that opened it"):
    assertEquals(Menu.anchor(buttonRightPx = 352.0, buttonBottomPx = 68.0, viewportWidthPx = 390.0), Menu.Anchor(68.0, 38.0))

  test("menu coordinates never escape above or to the right of the viewport"):
    assertEquals(Menu.anchor(buttonRightPx = 410.0, buttonBottomPx = -4.0, viewportWidthPx = 390.0), Menu.Anchor(0.0, 0.0))
