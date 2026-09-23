package sgrv.fe

import munit.FunSuite

final class SettingsPanelSuite extends FunSuite:
  test("a completed number preserves the value typed between stepper increments"):
    assertEquals(SettingsPanel.completedNumber("1.23"), Some(1.23))
    assertEquals(SettingsPanel.completedNumber("0.0125"), Some(0.0125))

  test("unfinished or invalid number text does not replace the draft value"):
    assertEquals(SettingsPanel.completedNumber(""), None)
    assertEquals(SettingsPanel.completedNumber("0"), None)
    assertEquals(SettingsPanel.completedNumber("not a number"), None)
    assertEquals(SettingsPanel.completedNumber("NaN"), None)
