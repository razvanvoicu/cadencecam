package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import sgrv.api.{AcquirerPresence, WorkoutState}
import sgrv.be.BackendCapabilities
import sgrv.be.auth.{SessionAuth, SessionStore}
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
  * and let it be this device. It does not open a workout; Start and Stop alone own that lifecycle.
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
      case RequestContext.Authenticated(request, user) =>
        counterLease(request)
          .zipWith(ZIO.service[Firestore])((lease, firestore) => lease -> firestore)
          .flatMap((lease, firestore) => AccountSessions.accountState(firestore, user.account, lease))
          .map: state =>
            val account = state.map(_._2)
            noStore(
              Response.json(
                AcquirerPresence(account.exists(_.counter.isDefined), account.flatMap(_.counterInstance)).toJson
              )
            )
          .catchAll: error =>
            ZIO.logWarningCause("Could not read the account's counter presence", Cause.fail(error)) *>
              // Absence now means an expired lease and makes the Counter log out. A failed read must therefore remain
              // a failure; disguising it as absence would turn a transient Firestore outage into a forced logout.
              ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))
      case _ => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))

  /** Takes the counter's role for the browser session asking. Idempotent, and a takeover rather than a new workout. */
  private def take(request: Request): ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO.serviceWithZIO[RequestContext]:
      case RequestContext.Authenticated(_, user) =>
        CountingSessionListener.browserSession(request) match
          // Unreachable while the request is authenticated, since authentication is the cookie; answered rather than
          // assumed away, because opening no record is the one outcome that must not look like success.
          case None =>
            ZIO
              .logWarning(s"A counter for ${user.email} carries no session cookie; no record opened")
              .as(noStore(Response.status(Status.BadRequest)))
          case Some(browserSession) =>
            counterLease(request).flatMap(lease =>
              open(user.account, browserSession, instanceId(request), lease.flatMap(_.expiresAt))
            )
      case _ => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))

  private def open(
      email: String,
      browserSession: String,
      instanceId: Option[String],
      sessionExpiresAt: Option[java.time.Instant]
  ): ZIO[Requires, Nothing, Response] =
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
              state <- keeper.takeCounter(named, browserSession, instanceId, sessionExpiresAt, now)
              _ <- ZIO.logInfo(s"Counter role taken for $email")
            yield noStore(Response.json(WorkoutState(state.active.isDefined).toJson))
      yield response
    taken.catchAll: error =>
      ZIO.logWarningCause("Could not take the counting role", Cause.fail(error)) *>
        ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)

  private def instanceId(request: Request): Option[String] =
    request.headers.get(AcquirerPresence.InstanceHeader).map(_.trim).filter(_.nonEmpty).map(_.take(200))

  private def counterLease(request: Request): ZIO[SessionStore, Nothing, Option[CounterLease]] =
    SessionAuth.sessionKey(request) match
      case None             => ZIO.none
      case Some(sessionKey) =>
        SessionStore
          .expiresAt(sessionKey)
          .map(expiry => Some(CounterLease(CountingSessionListener.browserSession(sessionKey), expiry)))
          .catchAll: error =>
            ZIO.logWarningCause("Could not read the Counter session expiry", Cause.fail(error)).as(None)
