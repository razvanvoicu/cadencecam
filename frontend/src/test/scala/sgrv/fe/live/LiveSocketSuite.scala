package sgrv.fe.live

import munit.FunSuite

class LiveSocketSuite extends FunSuite:

  test("a retry backs off, but not far"):
    // The usual cause of a drop is the platform closing a long-lived request rather than anything being broken, so
    // a workout must not pause for half a minute over a routine reconnection.
    assertEquals(LiveSocket.delayMillis(0), LiveSocket.FirstDelayMillis)
    assertEquals(LiveSocket.delayMillis(1), 1000)
    assertEquals(LiveSocket.delayMillis(2), 2000)
    assert(LiveSocket.delayMillis(20) <= LiveSocket.LongestDelayMillis)
    assert(LiveSocket.delayMillis(1000) <= LiveSocket.LongestDelayMillis, "the shift overflowed")

  test("the socket follows the scheme the page was served over"):
    // A secure page may not open an insecure socket: a browser refuses it outright rather than downgrading.
    assertEquals(LiveSocket.addressOf("/ws/dashboard", "https://example.run.app"), "wss://example.run.app/ws/dashboard")
    assertEquals(LiveSocket.addressOf("/ws/acquirer", "http://127.0.0.1:8888"), "ws://127.0.0.1:8888/ws/acquirer")
