package sgrv.fe

import munit.FunSuite

class DeviceSuite extends FunSuite:

  test("an Android phone names its model, which is the whole point of recording this"):
    // The field exists to tell one handset from another after the fact. Two low-end Samsungs and a Pixel produced
    // results so different that the detector looked inconsistent, and nothing in the record said which was which.
    val samsung = Some("Mozilla/5.0 (Linux; Android 14; SM-A536B) AppleWebKit/537.36 Chrome/128.0.0.0 Mobile Safari")

    assertEquals(Device.model(samsung), Some("Android 14, SM-A536B"))

  test("Linux is dropped from an Android agent, being the one part that says nothing"):
    val pixel = Some("Mozilla/5.0 (Linux; Android 15; Pixel 8) AppleWebKit/537.36 Chrome/129.0 Mobile Safari/537.36")

    assertEquals(Device.model(pixel), Some("Android 15, Pixel 8"))

  test("an iPhone says only that it is an iPhone, and that is worth knowing in advance"):
    // Safari on iOS reports no model, deliberately, and no amount of asking changes it. Better to know that when
    // reading the field than to discover it while trying to tell two handsets apart.
    val iphone = Some("Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 Version/18.0")

    assertEquals(Device.model(iphone), Some("iPhone, CPU iPhone OS 18_0 like Mac OS X"))

  test("an agent with nothing in brackets is kept whole rather than reduced to nothing"):
    assertEquals(Device.model(Some("CadenceCamBot/1.0")), Some("CadenceCamBot/1.0"))
    assertEquals(Device.model(None), None)

  test("the build stamp is read from where the build writes it"):
    // The key is shared with a line build.sbt prepends to main.js; if the two drift, the About panel silently shows
    // nothing and a stale bundle goes on looking like a fresh one.
    assertEquals(Device.BuildKey, "cadencecam.frontendBuild")
