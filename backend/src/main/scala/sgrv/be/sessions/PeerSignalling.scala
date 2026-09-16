package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import java.time.Instant
import sgrv.api.{PeerRole, PeerSignal, PeerSignals}
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, Clock, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

/** The post box two browsers use to find each other before talking directly.
  *
  * Everything here is filed under the account's counting session, which both devices resolve from being signed in --
  * so neither needs to know anything about the other, and neither needs to land in the same server process. That is
  * the whole reason this exists: once the link is up, the readings never touch the server again.
  *
  * Nothing here reads what it carries. An offer, an answer and a handful of candidates are opaque strings as the
  * browser produced them, filed and handed to the other end.
  */
object PeerSignalling extends BackendPlugin:
  type Requires = Firestore & SessionStore

  override val id = "peer-signalling"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.firestore) ++ CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(
      Method.POST / "live" / "signal" -> handler((request: Request) => post(request)),
      Method.GET / "live" / "signal" -> handler((request: Request) => waiting(request))
    )

  private def asUser[A](
      request: Request,
      act: (String, Request) => ZIO[Requires, Nothing, Response]
  ): ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO.serviceWithZIO[RequestContext]:
      case RequestContext.Authenticated(_, user) => act(user.email, request)
      case _                                     => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))

  private def post(request: Request): ZIO[Requires & RequestContext, Nothing, Response] = asUser(request, file)
  private def waiting(request: Request): ZIO[Requires & RequestContext, Nothing, Response] = asUser(request, hand)

  private def file(email: String, request: Request): ZIO[Requires, Nothing, Response] =
    val filed =
      for
        body <- request.body.asString
        signal <- ZIO.fromEither(body.fromJson[PeerSignal]).mapError(details => IllegalArgumentException(details))
        firestore <- ZIO.service[Firestore]
        located <- AccountSessions.active(firestore, email)
        // No session means nothing is counting, so there is nobody to pair with and nothing to file.
        response <- located match
          case None => ZIO.succeed(noStore(Response.status(Status.Conflict)))
          case Some((account, session)) =>
            for
              keeper <- AccountSessions.store(firestore)
              now <- Clock.instant
              _ <- keeper.postSignal(account, session, signal.from.toString, signal.kind, signal.body, now)
            yield noStore(Response.status(Status.NoContent))
      yield response
    filed.foldZIO(
      error =>
        ZIO.logWarningCause("Refused a peer signal", Cause.fail(error)) *>
          ZIO.succeed(noStore(Response.status(Status.BadRequest))),
      ZIO.succeed(_)
    )

  private def hand(email: String, request: Request): ZIO[Requires, Nothing, Response] =
    val asked =
      for
        mine <- ZIO.succeed(
          if request.url.queryParams.queryParam("role").contains(PeerRole.Counter.toString) then PeerRole.Counter
          else PeerRole.Watcher
        )
        since <- ZIO.succeed(
          request.url.queryParams.queryParam("since").flatMap(value => scala.util.Try(Instant.parse(value)).toOption)
        )
        firestore <- ZIO.service[Firestore]
        located <- AccountSessions.active(firestore, email)
        answer <- located match
          case None => ZIO.succeed(PeerSignals())
          case Some((account, session)) =>
            for
              keeper <- AccountSessions.store(firestore)
              now <- Clock.instant
              found <- keeper.signalsFor(account, session, mine.toString, since, now)
              (theirs, cursor) = found
            yield PeerSignals(
              theirs.map((kind, body) =>
                PeerSignal(if mine == PeerRole.Watcher then PeerRole.Counter else PeerRole.Watcher, kind, body)
              ),
              cursor
            )
      yield answer
    asked.foldZIO(
      error =>
        ZIO.logWarningCause("Could not read the peer post box", Cause.fail(error)) *>
          ZIO.succeed(noStore(Response.status(Status.BadRequest))),
      signals => ZIO.succeed(noStore(Response.json(signals.toJson)))
    )

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)
