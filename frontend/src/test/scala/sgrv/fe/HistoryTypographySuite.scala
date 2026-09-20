package sgrv.fe

class HistoryTypographySuite extends munit.FunSuite:

  test("a history line is fitted from the common measuring size with breathing room"):
    assertEquals(HistoryTypography.fittedPixels(180.0, 90.0), Some(19.2))

  test("history text remains within its legible size range"):
    assertEquals(HistoryTypography.legible(8.0), 9.5)
    assertEquals(HistoryTypography.legible(16.0), 16.0)
    assertEquals(HistoryTypography.legible(28.0), 20.0)
    assertEquals(HistoryTypography.cssPixels(16.0), "16.00px")

  test("a phone timestamp fills eighty percent and metadata is sixty percent of it"):
    assertEquals(HistoryTypography.filledPixels(180.0, 90.0), Some(16.0))
    assertEquals(HistoryTypography.filledPixelsAt(16.0, 180.0, 150.0), Some(15.36))
    assertEquals(HistoryTypography.secondaryPixels(16.0), 9.6)

  test("an element with no measurable width does not constrain the row"):
    assertEquals(HistoryTypography.fittedPixels(0.0, 90.0), None)
    assertEquals(HistoryTypography.fittedPixels(180.0, 0.0), None)
    assertEquals(HistoryTypography.filledPixels(0.0, 90.0), None)
    assertEquals(HistoryTypography.filledPixelsAt(0.0, 180.0, 90.0), None)
