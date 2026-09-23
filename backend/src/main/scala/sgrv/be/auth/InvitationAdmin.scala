package sgrv.be.auth

import sgrv.api.{InvitationBatch, InvitationCredentials, InvitationRevocation}
import sgrv.be.BackendCapabilities
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.{Cause, Clock, ZIO}
import zio.http.{Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

/** Owner-only creation and revocation of invitations.
  *
  * Codes are generated here, returned once, and never stored in plaintext. Creation uses a random suffix so two pools
  * with the same human prefix cannot overwrite each other; the returned usernames are what belong in review notes.
  */
object InvitationAdmin extends BackendPlugin:
  type Requires = InvitationAccounts & TokenGenerator

  override val id = "invitation-admin"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.invitationAccounts) ++
      CapabilitySet.one(BackendCapabilities.tokenGenerator)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.AdminPassword
  override val routes: Routes[Requires & RequestContext, Nothing] = Routes(
    Method.POST / "admin" / "invitations" -> handler((request: Request) => create(request)),
    Method.DELETE / "admin" / "invitations" -> handler((request: Request) => revoke(request))
  )

  private def create(request: Request): ZIO[Requires, Nothing, Response] =
    val created = for
      body <- request.body.asString
      requested <- ZIO.fromEither(body.fromJson[InvitationBatch]).mapError(new IllegalArgumentException(_))
      prefix <- ZIO.attempt(InvitationAccounts.validateUsername(requested.prefix))
      _ <- ZIO.unless(requested.count >= 1 && requested.count <= 20)(
        ZIO.fail(new IllegalArgumentException("Invitation count must be from 1 to 20"))
      )
      seeds <- ZIO.foreach(1 to requested.count): _ =>
        for
          suffix <- TokenGenerator.generate(5)
          code <- TokenGenerator.generate(24)
          account <- TokenGenerator.generate(18)
        yield NewInvitation(
          username = s"$prefix-${suffix.toLowerCase}",
          accessCode = code,
          accountId = s"invitation:$account",
          displayName = prefix
        )
      now <- Clock.instant
      credentials <- InvitationAccounts.create(seeds, now)
    yield noStore(Response.json(InvitationCredentials(credentials).toJson))

    created.catchAll: error =>
      ZIO.logWarningCause("Could not create invitations", Cause.fail(error)) *>
        ZIO.succeed(noStore(Response.text(error.getMessage).status(Status.BadRequest)))

  private def revoke(request: Request): ZIO[Requires, Nothing, Response] =
    val revoked = for
      body <- request.body.asString
      requested <- ZIO.fromEither(body.fromJson[InvitationRevocation]).mapError(new IllegalArgumentException(_))
      now <- Clock.instant
      _ <- InvitationAccounts.revoke(requested.username, now)
    yield noStore(Response.status(Status.NoContent))

    revoked.catchAll: error =>
      ZIO.logWarningCause("Could not revoke an invitation", Cause.fail(error)) *>
        ZIO.succeed(noStore(Response.status(Status.ServiceUnavailable)))

  private def noStore(response: Response): Response = response.addHeader(Header.CacheControl.NoStore)
