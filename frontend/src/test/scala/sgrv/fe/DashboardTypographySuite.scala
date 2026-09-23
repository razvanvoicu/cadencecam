package sgrv.fe

class DashboardTypographySuite extends munit.FunSuite:

  test("a single-line fit is expressed as viewport-width units with breathing room"):
    assertEquals(DashboardTypography.fittedVw(200.0, 50.0), Some(3.76))
    assertEquals(DashboardTypography.cssVw(3.76), "3.7600vw")

  test("an element with no measurable width does not constrain the shared size"):
    assertEquals(DashboardTypography.fittedVw(0.0, 50.0), None)
    assertEquals(DashboardTypography.fittedVw(200.0, 0.0), None)

  test("the card height caps short text before it can overlap another grid row"):
    assertEquals(DashboardTypography.heightLimitVw(400.0, 1000.0, 0.10), Some(4.0))
    assertEquals(DashboardTypography.heightLimitVw(0.0, 1000.0, 0.10), None)

  test("a headline is limited by the smaller of its grid row's width and height"):
    assertEquals(
      DashboardTypography.fittedPixels(600.0, 120.0, 500.0, 100.0, 100.0),
      Some(112.8)
    )
    assertEquals(
      DashboardTypography.fittedPixels(300.0, 400.0, 500.0, 100.0, 100.0),
      Some(56.4)
    )

  test("an unlaid-out headline keeps its CSS fallback"):
    assertEquals(DashboardTypography.fittedPixels(0.0, 120.0, 500.0, 100.0, 100.0), None)
