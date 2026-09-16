package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import sgrv.api.AcquirerPresence
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, ZIO}
import zio.http.{Header, Method, Response, Routes, Status, handler}
import zio.json.*

/** Whether this account already has something counting.
  *
  * Asked before a device takes the role, so displacing another one is a choice rather than a surprise. Read from the
  * account's record, which is the only place that knows: the answer must not depend on which server process the
  * counting device's requests happen to reach.
  */
object AcquirerPresenceRoute extends BackendPlugin:
  type Requires = Firestore & SessionStore

  override val id = "acquirer-presence"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.firestore) ++ CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.GET / "live" / "acquirer" -> handler(answer))

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

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)
