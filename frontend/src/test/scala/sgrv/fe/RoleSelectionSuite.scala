package sgrv.fe

import munit.FunSuite
import sgrv.api.AcquirerPresence

class RoleSelectionSuite extends FunSuite:

  test("an existing counter from this installation is not a takeover"):
    assert(RoleSelection.belongsToThisInstance(AcquirerPresence(true, Some("this-app")), "this-app"))

  test("a counter from another installation is a takeover"):
    assert(!RoleSelection.belongsToThisInstance(AcquirerPresence(true, Some("other-app")), "this-app"))

  test("legacy or absent ownership never masquerades as this installation"):
    assert(!RoleSelection.belongsToThisInstance(AcquirerPresence(true), "this-app"))
    assert(!RoleSelection.belongsToThisInstance(AcquirerPresence(false, Some("this-app")), "this-app"))
    assert(!RoleSelection.belongsToThisInstance(AcquirerPresence(true, Some("this-app")), ""))
