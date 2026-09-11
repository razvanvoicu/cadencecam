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

  test("a capture command carries the run that asked for it, over the wire"):
    // Without this a recording is identifiable only by the moment it arrived, which cannot tell one run from the
    // next when several are made under several accounts.
    val asked: sgrv.api.LiveCommand = sgrv.api.LiveCommand.CaptureTrace(Some("bench 123-abc: disc then bar"))

    val wire = zio.json.EncoderOps(asked).toJson
    assertEquals(zio.json.DecoderOps(wire).fromJson[sgrv.api.LiveCommand], Right(asked))
    assert(wire.contains("123-abc"), s"the run is not on the wire: $wire")

  test("a capture asked for by hand still needs no note"):
    val asked: sgrv.api.LiveCommand = sgrv.api.LiveCommand.CaptureTrace()

    assertEquals(zio.json.DecoderOps(zio.json.EncoderOps(asked).toJson).fromJson[sgrv.api.LiveCommand], Right(asked))

  test("giving one command a field changed how every command is written"):
    // Worth pinning rather than discovering later. A parameterless enum encodes as a bare string; once any case
    // carries a field the whole enum is written as an object, so Reset went from "Reset" to {"Reset":{}}. Both ends
    // ship together so they agree, but a device left on an older bundle will not understand either command.
    val reset: sgrv.api.LiveCommand = sgrv.api.LiveCommand.Reset

    assertEquals(zio.json.EncoderOps(reset).toJson, """{"Reset":{}}""")
    assertEquals(zio.json.DecoderOps("""{"Reset":{}}""").fromJson[sgrv.api.LiveCommand], Right(reset))
    assert(zio.json.DecoderOps("\"Reset\"").fromJson[sgrv.api.LiveCommand].isLeft, "the old form should not parse")
