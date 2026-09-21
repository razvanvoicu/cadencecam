package sgrv.be.auth

import zio.{Cause, Clock, ZIO}
import zio.http.{Header, Request, Response, Status}

/** Resolves an opaque browser-cookie or native-bearer session into its signed-in user. Used by [[Me]] to report the
  * session to the frontend and by authenticated access policies to authorize plugins and construct their request
  * context.
  */
private[be] object SessionAuth:

  def resolve(request: Request): ZIO[SessionStore, Nothing, Either[Response, SessionUser]] =
    val sessionUser = for
      now <- Clock.instant
      user <- sessionKey(request) match
        case Some(key) => SessionStore.find(key, now)
        case None      => ZIO.succeed(None)
    yield user
    sessionUser.foldZIO(
      error =>
        ZIO.logErrorCause("Could not resolve the browser session", Cause.fail(error)) *>
          ZIO.succeed(Left(Response.status(Status.ServiceUnavailable))),
      {
        case Some(user) => ZIO.succeed(Right(user))
        case None       => ZIO.succeed(Left(Response.status(Status.Unauthorized)))
      }
    )

  /** The opaque application session from either native bearer authentication or the browser cookie.
    *
    * Bearer wins when both are present: a native request should never be identified by an unrelated cookie a platform
    * HTTP stack happened to attach, while the browser continues down the cookie-only path it always used.
    */
  private[be] def sessionKey(request: Request): Option[String] =
    val bearer = request.headers.get(Header.Authorization).collect:
      case Header.Authorization.Bearer(token) => token.value.asString.trim
    bearer.filter(_.nonEmpty).orElse(
      request.cookie(Callback.sessionCookieName).map(_.content).map(_.trim).filter(_.nonEmpty)
    )
