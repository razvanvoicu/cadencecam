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
