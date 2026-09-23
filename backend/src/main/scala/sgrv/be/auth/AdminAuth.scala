package sgrv.be.auth

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import zio.{Cause, System, ZIO}
import zio.http.{Request, Response, Status}

/** Verifies the admin credential against `ADMIN_PASSWORD` for protected plugins.
  *
  * The header is the normal interface: unlike a query parameter it is not copied into Cloud Run's request URL logs or
  * browser history. `?pwd=` remains accepted for the existing local debug pages which are opened by hand.
  */
private[be] object AdminAuth:
  val HeaderName = "X-CadenceCam-Admin-Password"

  def reject(request: Request): ZIO[Any, Nothing, Option[Response]] =
    System
      .env("ADMIN_PASSWORD")
      .foldZIO(
        error =>
          ZIO.logErrorCause("Could not read ADMIN_PASSWORD", Cause.fail(error)) *>
            ZIO.succeed(Some(Response.status(Status.ServiceUnavailable))),
        configured =>
          configured.map(_.trim).filter(_.nonEmpty) match
            case None =>
              ZIO.logWarning("ADMIN_PASSWORD is not configured; admin-password-protected plugins are unavailable") *>
                ZIO.succeed(Some(Response.status(Status.ServiceUnavailable)))
            case Some(password) =>
              val provided = request.headers.get(HeaderName).orElse(request.queryParam("pwd")).getOrElse("")
              val granted =
                provided.nonEmpty && MessageDigest.isEqual(provided.getBytes(UTF_8), password.getBytes(UTF_8))
              ZIO.succeed(Option.unless(granted)(Response.status(Status.Unauthorized)))
      )
