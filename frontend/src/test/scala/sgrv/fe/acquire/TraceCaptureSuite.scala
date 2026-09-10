package sgrv.fe.acquire

import munit.FunSuite
import sgrv.api.SignalTrace
import zio.json.*

class TraceCaptureSuite extends FunSuite:

  private def filled(samples: Int): QuadrantSignals =
    val signals = QuadrantSignals()
    for index <- 0 until samples do
      signals.record(Sample(index.toDouble, Quadrant.All.map(q => q -> (120.0 + q.ordinal + index % 7)).toMap))
    signals

  test("a recording carries every channel, oldest sample first"):
    val trace = TraceCapture.of(filled(50), reps = 12, LockState.Searching, note = None)

    assertEquals(trace.samples.keySet, Quadrant.All.map(_.toString).toSet)
    assertEquals(trace.samples("Q1").size, 50)
    // Oldest first: the first sample recorded is the first in the trace.
    assertEquals(trace.samples("Q1").head, 120.0 + Quadrant.Q1.ordinal)
    assertEquals(trace.reps, 12)

  test("a recording is stamped with the rate the detector actually samples at"):
    val trace = TraceCapture.of(filled(10), reps = 0, LockState.Searching, note = None)

    assertEquals(trace.sampleRateHz, DetectorSettings().sampleRateHz)

  test("the buffer is taken whole, since what matters usually happened before the button was reached for"):
    val trace = TraceCapture.of(filled(QuadrantSignals.OneMinute + 200), 5, LockState.Searching, None)

    assertEquals(trace.samples("Q1").size, QuadrantSignals.OneMinute)

  test("the lock is recorded in words, so a replay knows what the detector believed at the time"):
    assertEquals(TraceCapture.describe(LockState.Searching), "searching")
    assertEquals(TraceCapture.describe(LockState.Acquiring(30, 150)), "acquiring 30/150")
    assertEquals(TraceCapture.describe(LockState.Locked(Quadrant.Q4, Quadrant.Q2, 1.0)), "locked Q4+Q2 1.00s")

  test("an empty note is left out rather than stored as emptiness"):
    assertEquals(TraceCapture.of(filled(10), 0, LockState.Searching, Some("   ")).note, None)
    assertEquals(TraceCapture.of(filled(10), 0, LockState.Searching, Some(" curls ")).note, Some("curls"))

  test("nothing recorded is not worth sending"):
    assert(!TraceCapture.worthSending(QuadrantSignals()))
    assert(TraceCapture.worthSending(filled(1)))

  test("a recording round-trips through the wire format the backend parses"):
    val trace = TraceCapture.of(filled(20), 7, LockState.Locked(Quadrant.Q1, Quadrant.Q3, 1.25), Some("test"))

    assertEquals(trace.toJson.fromJson[SignalTrace], Right(trace))

  test("a trace says what became of the camera's controls, in words"):
    assertEquals(TraceCapture.describe(ControlOutcome("switched off")), "switched off; held: none")
    assertEquals(
      TraceCapture.describe(ControlOutcome("this browser does not report camera capabilities")),
      "this browser does not report camera capabilities; held: none"
    )
    assertEquals(
      TraceCapture.describe(ControlOutcome("attempted", Seq("exposureMode"), Seq("focusMode"))),
      "attempted; held: exposureMode, left automatic: focusMode"
    )

  test("a trace records the sample at which the controls settled"):
    // In the same record as the signal, so the picture changing and the controls settling can be read against each
    // other rather than against two clocks.
    val trace = TraceCapture.of(
      filled(40),
      reps = 0,
      LockState.Searching,
      note = None,
      camera = Some("""{"hasGetCapabilities":false}"""),
      controls = Some("switched off; held: none"),
      controlsAtSample = Some(15)
    )

    assertEquals(trace.controlsAtSample, Some(15))
    assertEquals(trace.controls, Some("switched off; held: none"))
    assertEquals(trace.camera, Some("""{"hasGetCapabilities":false}"""))
