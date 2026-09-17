package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import sgrv.api.AcquirerPresence
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, Clock, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

/** Whether this account already has something counting, and the taking of that role.
  *
  * Asked before a device takes the role, so displacing another one is a choice rather than a surprise. Read from the
  * account's record, which is the only place that knows: the answer must not depend on which server process the
  * counting device's requests happen to reach.
  *
  * Taking the role lives here too, on the same path, because the two are one question asked twice — who is counting,
  * and let it be this device. It used to be done by the relay's socket, and when the relay went the taking went with
  * it: nothing opened the account's session any more, so every later request that needed one found none.
  */
object AcquirerPresenceRoute extends BackendPlugin:
  type Requires = Firestore & SessionStore

  override val id = "acquirer-presence"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.firestore) ++ CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(
      Method.GET / "live" / "acquirer" -> handler(answer),
      Method.POST / "live" / "acquirer" -> handler((request: Request) => take(request))
    )

  private def answer: ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO.serviceWithZIO[RequestContext]:
      case RequestContext.Authenticated(_, user) =>
        ZIO
          .serviceWithZIO[Firestore](firestore => AccountSessions.active(firestore, user.email))
          .catchAll: error =>
            ZIO.logWarningCause(
              "Could not read the account's session; reporting nothing counting",
              Cause.fail(error)
            ) *>
              ZIO.none
          .map(active => noStore(Response.json(AcquirerPresence(active.isDefined).toJson)))
      case _ => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))

  /** Takes the counter's role for the browser session asking, opening the account's session if it has none.
    *
    * Idempotent, and a takeover rather than a new session: a second device arriving moves the role within the session
    * the account already has, so a set in progress is not split in two. The device it displaces learns of it from its
    * next progress report being refused.
    */
  private def take(request: Request): ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO.serviceWithZIO[RequestContext]:
      case RequestContext.Authenticated(_, user) =>
        CountingSessionListener.documentId(request) match
          // Unreachable while the request is authenticated, since authentication is the cookie; answered rather than
          // assumed away, because opening no record is the one outcome that must not look like success.
          case None =>
            ZIO
              .logWarning(s"A counter for ${user.email} carries no session cookie; no record opened")
              .as(noStore(Response.status(Status.BadRequest)))
          case Some(browserSession) => open(user.email, browserSession)
      case _ => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))

  private def open(email: String, browserSession: String): ZIO[Requires, Nothing, Response] =
    val taken =
      for
        firestore <- ZIO.service[Firestore]
        name <- AccountKey.of(email)
        response <- name match
          // The key is what names the document; without it there is nowhere safe to write, and that is a fault in the
          // deployment rather than in the request.
          case None        => ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))
          case Some(named) =>
            for
              keeper <- AccountSessions.store(firestore)
              now <- Clock.instant
              session <- keeper.takeCounting(named, email, browserSession, now)
              _ <- ZIO.logInfo(s"Counting for $email in session $session")
            yield noStore(Response.status(Status.NoContent))
      yield response
    taken.catchAll: error =>
      ZIO.logWarningCause("Could not take the counting role", Cause.fail(error)) *>
        ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)
