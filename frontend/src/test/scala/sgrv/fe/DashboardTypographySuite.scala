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
