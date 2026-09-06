package sgrv.be.auth

import sgrv.be.BackendCapabilities
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import sgrv.api.CurrentUser
import zio.ZIO
import zio.http.{Header, Method, Request, Response, Routes, handler}
import zio.json.*

/** Resolves the opaque browser session cookie through Firestore and reports its user to the frontend. */
object Me extends BackendPlugin:
  type Requires = SessionStore

  override val id = "auth-me"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Public
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.GET / "me" -> handler((request: Request) => apply(request)))

  private def apply(request: Request): ZIO[Requires, Nothing, Response] =
    SessionAuth
      .resolve(request)
      .map:
        case Right(user)    => Response.json(json(user))
        case Left(response) => response
      .map(_.addHeader(Header.CacheControl.NoStore))

  private[auth] def json(user: SessionUser): String = CurrentUser(user.email, user.name).toJson
