package sgrv.be.about

import sgrv.api.AboutInfo
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.http.{Header, Method, Response, Routes, handler}
import zio.json.*

/** Returns metadata captured when this backend was compiled. */
object About extends BackendPlugin:
  type Requires = SessionStore

  override val id = "about"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.GET / "about" -> handler(response(BuildInformation.current)))

  private[about] def response(information: AboutInfo): Response =
    Response.json(information.toJson).addHeader(Header.CacheControl.NoStore)
