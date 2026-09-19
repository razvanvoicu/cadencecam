package sgrv.fe

import munit.FunSuite
import org.scalajs.dom

import scala.scalajs.js

class StartPreferenceSuite extends FunSuite:

  /** A browser's storage, as far as these functions use one: three methods over a dictionary. */
  private def storage(): (dom.Storage, js.Dictionary[String]) =
    val data = js.Dictionary.empty[String]
    val fake = js.Dynamic
      .literal(
        getItem = ((key: String) => data.getOrElse(key, null)): js.Function1[String, String],
        setItem = ((key: String, value: String) => data(key) = value): js.Function2[String, String, Unit],
        removeItem = ((key: String) => { val _ = data.remove(key) }): js.Function1[String, Unit]
      )
      .asInstanceOf[dom.Storage]
    (fake, data)

  test("a device told to default to the dashboard opens there, at every start"):
    assertEquals(StartPreference.opening(defaultToDashboard = true, presentRolesOnce = false), Some(Screen.Dashboard))

  test("a device with no preference leaves the usual rules in charge"):
    // Those rules are what keep a counting phone counting across a reload: it reopens the role it already held.
    assertEquals(StartPreference.opening(defaultToDashboard = false, presentRolesOnce = false), None)

  test("the start after the preference is turned off presents the roles"):
    assertEquals(StartPreference.opening(defaultToDashboard = false, presentRolesOnce = true), Some(Screen.Selection))

  test("the preference is stored exactly as asked: the key present with true, or absent"):
    val (fake, data) = storage()

    StartPreference.setDefaultsToDashboard(fake, on = true)
    assertEquals(data.get(StartPreference.DashboardKey), Some("true"))
    assert(StartPreference.defaultsToDashboard(fake))

    StartPreference.setDefaultsToDashboard(fake, on = false)
    assertEquals(data.get(StartPreference.DashboardKey), None, "turning it off undoes the save")
    assert(!StartPreference.defaultsToDashboard(fake))

  test("turning it off changes the next start, and only that one"):
    // Without the marker a device reloading on the dashboard would reopen the dashboard -- the screen it was last on --
    // and "off" would change nothing anybody could see.
    val (fake, _) = storage()
    StartPreference.setDefaultsToDashboard(fake, on = true)
    StartPreference.setDefaultsToDashboard(fake, on = false)

    assertEquals(StartPreference.consumeOpening(fake), Some(Screen.Selection))
    assertEquals(StartPreference.consumeOpening(fake), None, "the start after that is back to the usual rules")

  test("turning it back on forgets that it was ever turned off"):
    val (fake, data) = storage()
    StartPreference.setDefaultsToDashboard(fake, on = false)
    StartPreference.setDefaultsToDashboard(fake, on = true)

    assertEquals(data.get(StartPreference.PresentRolesOnceKey), None)
    assertEquals(StartPreference.consumeOpening(fake), Some(Screen.Dashboard))
    assertEquals(StartPreference.consumeOpening(fake), Some(Screen.Dashboard), "and it stays on")

  test("a device that never touched the preference is left exactly as it was"):
    val (fake, data) = storage()

    assertEquals(StartPreference.consumeOpening(fake), None)
    assert(data.isEmpty, "reading must not write anything")
