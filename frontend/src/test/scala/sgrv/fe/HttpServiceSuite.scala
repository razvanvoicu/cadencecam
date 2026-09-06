package sgrv.fe

import munit.FunSuite

import java.util.concurrent.atomic.AtomicInteger

class HttpServiceSuite extends FunSuite:

  test("reports every unauthorized response without owning session state"):
    val unauthorizedCalls = AtomicInteger(0)
    val service = HttpService(() =>
      val _ = unauthorizedCalls.incrementAndGet()
      ()
    )

    service.observeStatus(401)
    assertEquals(unauthorizedCalls.get(), 1)

    service.observeStatus(500)
    service.observeStatus(401)
    service.observeStatus(401)
    assertEquals(unauthorizedCalls.get(), 3)
