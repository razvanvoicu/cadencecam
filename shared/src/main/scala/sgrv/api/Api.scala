package sgrv.api

import zio.json.{DeriveJsonCodec, JsonCodec, jsonNoExtraFields}
import zio.json.ast.Json

/** The signed-in user as `/me` reports them.
  *
  * `extra` carries whatever the application's own `CurrentUserContributor`s added, keyed by contributor id, so an
  * application can put its session data in front of the frontend without a second request and without the template
  * growing fields only one application uses. It is absent, not empty, when nothing contributed — an application with no
  * contributors sees exactly the payload this route always returned.
  */
@jsonNoExtraFields
final case class CurrentUser(email: String, name: String, extra: Option[Map[String, Json]] = None)

object CurrentUser:
  given JsonCodec[CurrentUser] = DeriveJsonCodec.gen[CurrentUser]

@jsonNoExtraFields
final case class AboutInfo(
    appVersion: String,
    buildDate: String,
    buildOs: String,
    scalaVersion: String,
    scalaJsVersion: String
)

object AboutInfo:
  given JsonCodec[AboutInfo] = DeriveJsonCodec.gen[AboutInfo]

/** What the backend's counting-session contributor files under its key in [[CurrentUser.extra]]. */
@jsonNoExtraFields
final case class CountingSession(sessionId: String)

object CountingSession:
  /** The contributor's id, which is also the key its entry appears under. Shared so the two ends cannot drift. */
  val Key = "counting-session"
  given JsonCodec[CountingSession] = DeriveJsonCodec.gen[CountingSession]

/** How far the acquirer has counted, reported periodically into its counting session.
  *
  * Only the total: which device counted it, and whether a second acquirer should carry on from it, are questions the
  * dashboard will need answered but this record does not yet decide.
  */
@jsonNoExtraFields
final case class RepProgress(reps: Int)

object RepProgress:
  /** Where the acquirer reports to. Shared so the route and the caller cannot drift apart. */
  val Path = "/countingSession/reps"
  given JsonCodec[RepProgress] = DeriveJsonCodec.gen[RepProgress]

/** A minute of the acquirer's four quadrant signals, as they were when captured.
  *
  * The detector's thresholds were all chosen by reasoning about signals nobody had looked at, and every guess made that
  * way so far has been wrong in a different direction. This is the raw material for settling them instead: a real
  * recording, of real lighting and a real movement, that can be replayed offline as many times as a question needs
  * asking.
  *
  * `samples` is keyed by quadrant name and ordered oldest first, at `sampleRateHz`. `note` is whatever the person
  * capturing it wants to remember about what they were doing, which is the one thing the numbers cannot recover.
  */
@jsonNoExtraFields
final case class SignalTrace(
    sampleRateHz: Double,
    samples: Map[String, Seq[Double]],
    reps: Int,
    lock: String,
    note: Option[String] = None,
    /** What the camera said it could do and where it sat, as JSON. Absent when the browser reports neither.
      *
      * Carried because these differ by device in ways that decide whether the controls can be held still at all, and
      * there is no other way to read them off a phone.
      */
    camera: Option[String] = None
)

object SignalTrace:
  val Path = "/countingSession/trace"
  given JsonCodec[SignalTrace] = DeriveJsonCodec.gen[SignalTrace]
