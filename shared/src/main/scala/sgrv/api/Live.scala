package sgrv.api

import zio.json.{DeriveJsonCodec, JsonCodec, jsonNoExtraFields}

/** What the acquiring device reports to whoever is watching it.
  *
  * The status line travels as the text the acquirer is itself showing rather than as a state to be re-rendered. The two
  * screens are meant to read identically, and the only way to guarantee that is for one of them to do the wording and
  * the other to repeat it.
  */
@jsonNoExtraFields
final case class LiveReading(
    reps: Int,
    repsPerMinute: Double,
    status: String,
    /** Which device is doing the counting, as far as its browser will say.
      *
      * Travels with the reading because the acquirer is the only one that knows it, and it is the one worth knowing. A
      * test event is filed by the bench, which recorded its own device readily enough -- and the bench is a laptop
      * showing an animation, not the handset whose camera and processor decide whether the reps are found.
      */
    device: Option[String] = None
)

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

  /** Stand down: another device has taken over counting for this account.
    *
    * The one message on this channel the server sends of its own accord rather than relaying from a dashboard. A
    * displaced acquirer used to have its socket closed under it and carry on regardless -- still showing a count, and
    * still holding the camera -- which on a desk full of phones is how two devices end up counting the same set and
    * neither of them says so.
    */
  case Displaced

  /** `note` says who asked and why, and ends up on the recording itself.
    *
    * Without it a capture is identifiable only by the moment it arrived, which is enough to tell one account's runs
    * from another's but not one run from the next -- and a recording that cannot be tied to the events logged beside it
    * is half the evidence.
    */
  case CaptureTrace(note: Option[String] = None)

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
    detail: Option[String] = None,
    /** Which device was counting, as far as the browser will say.
      *
      * Recorded because the answer to "why did this suite undercount" has turned out to depend on it more than on
      * anything in the detector: runs from the same account on different handsets have come back exactly right, one
      * short on every test, and fifteen short. Without this the only way to tell them apart afterwards was to ask
      * whoever ran them what was on the desk at the time.
      */
    device: Option[String] = None
)

object TestEvent:
  val Path = "/test/event"
  given JsonCodec[TestEvent] = DeriveJsonCodec.gen[TestEvent]

/** Whether this account already has a device counting for it.
  *
  * Asked before a device takes the acquirer's role, so taking over from another one is something the person chooses
  * rather than something that happens to them.
  */
@jsonNoExtraFields
final case class AcquirerPresence(acquiring: Boolean)

object AcquirerPresence:
  val Path = "/live/acquirer"
  given JsonCodec[AcquirerPresence] = DeriveJsonCodec.gen[AcquirerPresence]

object Live:
  /** Where each side connects. Shared so the two ends cannot drift apart. */
  val AcquirerPath = "/ws/acquirer"
  val DashboardPath = "/ws/dashboard"
