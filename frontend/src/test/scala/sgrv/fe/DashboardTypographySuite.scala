package sgrv.fe

class DashboardTypographySuite extends munit.FunSuite:

  test("a single-line fit is expressed as viewport-width units with breathing room"):
    assertEquals(DashboardTypography.fittedVw(200.0, 50.0), Some(3.76))
    assertEquals(DashboardTypography.cssVw(3.76), "3.7600vw")

  test("an element with no measurable width does not constrain the shared size"):
    assertEquals(DashboardTypography.fittedVw(0.0, 50.0), None)
    assertEquals(DashboardTypography.fittedVw(200.0, 0.0), None)
