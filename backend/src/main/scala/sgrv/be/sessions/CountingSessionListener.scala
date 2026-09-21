package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import sgrv.api.CountingSession
import sgrv.be.BackendCapabilities
import sgrv.be.core.{CapabilitySet, CurrentUserContributor, LoginEvent, LogoutEvent, RequestContext, SessionListener}
import sgrv.be.auth.SessionAuth
import zio.http.Request
import zio.ZIO
import zio.json.ast.Json

/** Records authentication events without turning them into workouts. Start and Stop own the ordinary workout
  * lifecycle; the logout route separately closes an open workout before it invalidates the account's credentials.
  */
object CountingSessionListener extends SessionListener:
  type Requires = Firestore

  override val id = "counting-session"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.one(BackendCapabilities.firestore)

  /** A login is an event, not a set. Every device that signs in reaches the same screens, and only Start creates a
    * workout history row.
    */
  override def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit] =
    record(AuthenticationEventType.Login, event.sessionKey, event.user.email, event.at)

  /** The route has already closed any active workout and invalidated every device before this event is raised. */
  override def onLogout(event: LogoutEvent): ZIO[Requires, Throwable, Unit] =
    record(AuthenticationEventType.Logout, event.sessionKey, event.user.email, event.at)

  private def record(
      eventType: AuthenticationEventType,
      sessionKey: String,
      email: String,
      at: java.time.Instant
  ): ZIO[Requires, Throwable, Unit] =
    for
      firestore <- ZIO.service[Firestore]
      account <- AccountKey.of(email)
      _ <- ZIO.foreachDiscard(account): name =>
        AccountSessions
          .store(firestore)
          .flatMap(_.recordAuthenticationEvent(name, eventType, browserSession(sessionKey), at))
    yield ()

  /** The identity the backend knows a client by, or `None` when the request carries no application session.
    *
    * What the account's record names as its counter, so a second device taking the role is a change of hand, and the
    * device it displaced can be told so. A digest of the session key rather than the key itself: the key is what
    * authenticates the client, and it is not copied into a second collection.
    */
  private[sessions] def browserSession(request: Request): Option[String] =
    SessionAuth.sessionKey(request).map(browserSession)

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
