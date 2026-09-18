package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, ZIO}
import zio.http.{Header, Method, Response, Routes, Status, handler}

/** Removes everything the account holds, when the account asks.
  *
  * One route rather than a sweep the app performs with several calls: what somebody asks for when they ask for this is
  * that nothing of theirs is left, and a client that had to name every place data is kept would be a client that
  * forgets one the next time somewhere new is written to. Everything that knows where an account's data lives is here.
  *
  * Scoped to whoever is signed in, and takes no argument at all: there is nothing to get wrong, and no id that could
  * name somebody else's account.
  */
object AccountDataRoute extends BackendPlugin:
  type Requires = Firestore & SessionStore

  override val id = "account-data"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.firestore) ++ CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.DELETE / "account" / "data" -> handler(forget))

  private def forget: ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO.serviceWithZIO[RequestContext]:
      case RequestContext.Authenticated(_, user) =>
        val removed =
          for
            firestore <- ZIO.service[Firestore]
            keeper <- AccountSessions.store(firestore)
            name <- AccountKey.of(user.email)
            response <- name match
              // Without the key there is no name to look under, and answering success would tell somebody their data
              // was gone when nothing had been read, let alone removed.
              case None        => ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))
              case Some(named) =>
                keeper.forgetEverything(named, user.email) *>
                  // Said out loud. Deleting everything somebody has is worth a line in the log precisely because it
                  // was asked for: it is the record that it happened, and that it happened on purpose.
                  ZIO.logWarning("Removed every record held for an account, at the account's request") *>
                  ZIO.succeed(noStore(Response.status(Status.NoContent)))
          yield response
        removed.catchAll: error =>
          ZIO.logWarningCause("Could not remove the account's data", Cause.fail(error)) *>
            ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))
      case _ => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)
