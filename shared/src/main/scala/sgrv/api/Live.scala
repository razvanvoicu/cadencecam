package sgrv.api

import zio.json.{DeriveJsonCodec, JsonCodec, jsonNoExtraFields}

/** What the acquiring device reports to whoever is watching it.
  *
  * The status line travels as the text the acquirer is itself showing rather than as a state to be re-rendered. The two
  * screens are meant to read identically, and the only way to guarantee that is for one of them to do the wording and
  * the other to repeat it.
  */
@jsonNoExtraFields
final case class LiveReading(reps: Int, repsPerMinute: Double, status: String)

object LiveReading:
  given JsonCodec[LiveReading] = DeriveJsonCodec.gen[LiveReading]

/** What a watching device is told: whether anything is counting for this account, and the last thing it said.
  *
  * Presence travels separately from the reading because the two answer different questions, and a dashboard that cannot
  * tell them apart can only say "waiting" to both — which is what it says when its own socket is down as well, leaving
  * three quite different situations looking identical.
  */
@jsonNoExtraFields
final case class LiveState(acquiring: Boolean, reading: Option[LiveReading] = None)

object LiveState:
  given JsonCodec[LiveState] = DeriveJsonCodec.gen[LiveState]

/** What a watching device asks the acquirer to do.
  *
  * Deliberately a closed set rather than anything resembling remote control: a dashboard may ask for the two things its
  * own buttons offer, and the acquirer decides what they mean.
  */
enum LiveCommand:
  case Reset
  case CaptureTrace

object LiveCommand:
  given JsonCodec[LiveCommand] = DeriveJsonCodec.gen[LiveCommand]

/** Something worth recording from a test run against the counter.
  *
  * Kept because the interesting failures are not visible in a final total. A counter that stalls for fifteen seconds
  * and then credits every missed rep at once ends a test with the right number, and so does one that quietly loses four
  * and gains four elsewhere; only a record of what happened while it happened tells those apart.
  */
@jsonNoExtraFields
final case class TestEvent(
    runId: String,
    testName: String,
    kind: String,
    reference: Int,
    acquired: Int,
    lagSeconds: Double,
    atSeconds: Double,
    detail: Option[String] = None
)

object TestEvent:
  val Path = "/test/event"
  given JsonCodec[TestEvent] = DeriveJsonCodec.gen[TestEvent]

object Live:
  /** Where each side connects. Shared so the two ends cannot drift apart. */
  val AcquirerPath = "/ws/acquirer"
  val DashboardPath = "/ws/dashboard"
