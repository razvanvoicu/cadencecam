package sgrv.be.auth

import sgrv.be.BackendCapabilities
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, CurrentUserDetails, RequestContext}
import sgrv.api.CurrentUser
import zio.ZIO
import zio.http.{Header, Method, Request, Response, Routes, handler}
import zio.json.*
import zio.json.ast.Json

/** Resolves the opaque browser session cookie through Firestore and reports its user to the frontend. */
object Me extends BackendPlugin:
  type Requires = SessionStore & CurrentUserDetails

  override val id = "auth-me"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.sessionStore) ++
      CapabilitySet.one(BackendCapabilities.currentUserDetails)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.GET / "me" -> handler((request: Request) => apply(request)))

  private def apply(request: Request): ZIO[Requires, Nothing, Response] =
    SessionAuth
      .resolve(request)
      .flatMap:
        case Left(response) => ZIO.succeed(response)
        case Right(user)    =>
          // Contributors ride along on the call the frontend already makes; a failing one omits its own key rather
          // than turning "who is signed in" into an error.
          CurrentUserDetails
            .extras(RequestContext.Authenticated(request, user))
            .map(extra => Response.json(json(user, extra)))
      .map(_.addHeader(Header.CacheControl.NoStore))

  private[auth] def json(user: SessionUser, extra: Map[String, Json] = Map.empty): String =
    CurrentUser(user.email, user.name, Option.when(extra.nonEmpty)(extra)).toJson
