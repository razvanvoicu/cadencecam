package sgrv.be.sessions

import com.google.cloud.firestore.Firestore
import sgrv.api.AccountSettings
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

/** What the account has set: a weight, a default factor, and the exercises it counts.
  *
  * Kept on the account's own document, beside the session it is in the middle of, because that is what these belong to.
  * A weight is not a property of a tablet, and a dashboard opened on a second device must report the same figures as
  * the first without anyone having to enter them twice.
  */
object AccountSettingsRoute extends BackendPlugin:
  type Requires = Firestore & SessionStore

  override val id = "account-settings"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.firestore) ++ CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(
      Method.GET / "account" / "settings" -> handler(read),
      Method.PUT / "account" / "settings" -> handler((request: Request) => write(request))
    )

  private def read: ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO.serviceWithZIO[RequestContext]:
      case RequestContext.Authenticated(_, user) =>
        val found =
          for
            firestore <- ZIO.service[Firestore]
            keeper <- AccountSessions.store(firestore)
            name <- AccountKey.of(user.email)
            settings <- name match
              case None        => ZIO.none
              case Some(named) => keeper.settings(named)
          yield settings
        found
          .catchAll: error =>
            // An account that cannot be read is not an account with no settings, but the difference is one the
            // dashboard cannot act on either way, and refusing to draw anything would be worse than drawing the
            // defaults. The fault is logged where it can be acted on.
            ZIO.logWarningCause("Could not read the account's settings", Cause.fail(error)) *> ZIO.none
          // Defaults rather than a 404: an account that has never been through the settings screen has settings, they
          // are simply the ones it started with, and every reader would otherwise have to invent them identically.
          .map(settings => noStore(Response.json(settings.getOrElse(AccountSettings.Initial).toJson)))
      case _ => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))

  private def write(request: Request): ZIO[Requires & RequestContext, Nothing, Response] =
    ZIO.serviceWithZIO[RequestContext]:
      case RequestContext.Authenticated(_, user) =>
        val saved =
          for
            body <- request.body.asString
            settings <- ZIO
              .fromEither(AccountSettingsRoute.parse(body))
              .mapError(details => IllegalArgumentException(details))
            firestore <- ZIO.service[Firestore]
            keeper <- AccountSessions.store(firestore)
            name <- AccountKey.of(user.email)
            response <- name match
              // The key is what names the document. Without it there is nowhere to write that is safe to write to,
              // and answering success would lose the settings silently.
              case None        => ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))
              case Some(named) =>
                keeper.saveSettings(named, settings).as(noStore(Response.json(settings.toJson)))
          yield response
        saved.catchAll:
          case invalid: IllegalArgumentException =>
            ZIO.logWarning(s"Rejected account settings: ${invalid.getMessage}") *>
              ZIO.succeed(noStore(Response.status(Status.BadRequest)))
          case error =>
            ZIO.logWarningCause("Could not save the account's settings", Cause.fail(error)) *>
              ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))
      case _ => ZIO.succeed(noStore(Response.status(Status.Unauthorized)))

  /** Settings from a request body, or why they are not settings.
    *
    * Checked rather than stored as sent. A weight of zero or a factor of zero makes every figure on the dashboard zero
    * for ever, which reads as the counter being broken; a negative one reads as it counting backwards. Neither is
    * something a person meant, so neither is written.
    */
  private[sessions] def parse(body: String): Either[String, AccountSettings] =
    body
      .fromJson[AccountSettings]
      .flatMap: settings =>
        if !(settings.weightKilograms > 0) then Left(s"Weight ${settings.weightKilograms} is not above zero")
        else if !(settings.defaultFactor > 0) then Left(s"Default factor ${settings.defaultFactor} is not above zero")
        else
          settings.exercises.find(exercise => !(exercise.factor > 0)) match
            case Some(exercise) => Left(s"Factor ${exercise.factor} for '${exercise.name}' is not above zero")
            case None           =>
              val ids = settings.exercises.map(_.id)
              if ids.distinct.sizeIs != ids.size then Left("Two exercises share an id")
              else if settings.exercises.exists(_.name.trim.isEmpty) then Left("An exercise has no name")
              else
                // A selection naming an exercise that is not there would leave the dashboard reporting the default
                // factor while the settings screen showed a chosen row; dropping it makes the two agree.
                Right(settings.copy(selected = settings.selected.filter(ids.contains)))

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)
