package sgrv.be

import sgrv.be.auth.{GoogleOAuth, SessionStore, TokenGenerator}
import sgrv.be.core.Capability
import zio.http.Client

/** Services available to dynamically discovered backend routes. */
type BackendEnvironment = GoogleOAuth & SessionStore & TokenGenerator & Client

private[be] object BackendCapabilities:
  val googleOAuth: Capability[GoogleOAuth] = Capability("google-oauth")
  val sessionStore: Capability[SessionStore] = Capability("session-store")
  val tokenGenerator: Capability[TokenGenerator] = Capability("token-generator")
  val httpClient: Capability[Client] = Capability("http-client")
