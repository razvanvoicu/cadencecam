package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import sgrv.api.CountingSession
import sgrv.be.BackendCapabilities
import sgrv.be.core.{CapabilitySet, CurrentUserContributor, LoginEvent, LogoutEvent, RequestContext, SessionListener}
import sgrv.be.auth.Callback
import zio.http.Request
import zio.ZIO
import zio.json.ast.Json

/** Authentication events deliberately do not own the workout lifecycle. Start and Stop do; signing in and out only
  * changes whether this browser may issue those commands.
  */
object CountingSessionListener extends SessionListener:
  type Requires = Firestore

  override val id = "counting-session"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.one(BackendCapabilities.firestore)

  /** Nothing is opened at login.
    *
    * A login is not a set. Every device that signs in reaches the same screens, and only one of them counts; the
    * session is opened when a device takes the counter's role, which is the moment there is something to record.
    */
  override def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit] =
    ZIO.logInfo(s"Signed in: ${event.user.email}")

  /** Logout is not Stop. An active workout remains available for its counter to finish (or for the idle timeout to
    * close if that counter has gone away).
    */
  override def onLogout(event: LogoutEvent): ZIO[Requires, Throwable, Unit] =
    ZIO.logInfo(s"Signed out without changing workout state: ${event.user.email}")

  /** The identity the backend knows a browser by, or `None` when the request carries no session cookie.
    *
    * What the account's record names as its counter, so a second device taking the role is a change of hand, and the
    * device it displaced can be told so. A digest of the session key rather than the key itself: the key is what
    * authenticates the browser, and it is not copied into a second collection.
    */
  private[sessions] def browserSession(request: Request): Option[String] =
    request.cookie(Callback.sessionCookieName).map(_.content).map(_.trim).filter(_.nonEmpty).map(browserSession)

  private[sessions] def browserSession(sessionKey: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(sessionKey.getBytes(UTF_8))
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

/** Tells the frontend the identity the backend knows its browser by.
  *
  * Derived from the session cookie rather than read back from Firestore, so this costs nothing on a page load. It rides
  * along on `/me`, which the frontend already calls on every load, instead of needing a request of its own -- and the
  * key it is filed under matches this contributor's id.
  */
object CountingSessionContributor extends CurrentUserContributor:
  type Requires = Any

  override val id = CountingSession.Key
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty

  override def contribute(context: RequestContext.Authenticated): ZIO[Requires, Throwable, Option[Json]] =
    ZIO.succeed:
      CountingSessionListener
        .browserSession(context.request)
        .map(browser => Json.Obj("sessionId" -> Json.Str(browser)))
