package sgrv.be

import com.google.cloud.firestore.Firestore
import sgrv.be.auth.{GoogleOAuth, SessionStore, TokenGenerator}
import sgrv.be.core.{Capability, LoginNotifier}
import zio.http.Client

/** Services the host itself constructs. [[sgrv.be.core.LoginNotifier]] is deliberately absent: it is derived from this
  * environment at startup and joins the capability registry afterwards, so it cannot also be an input to it.
  */
type BackendEnvironment = GoogleOAuth & SessionStore & TokenGenerator & Client & Firestore

private[be] object BackendCapabilities:
  val googleOAuth: Capability[GoogleOAuth] = Capability("google-oauth")
  val sessionStore: Capability[SessionStore] = Capability("session-store")
  val tokenGenerator: Capability[TokenGenerator] = Capability("token-generator")
  val httpClient: Capability[Client] = Capability("http-client")
  val firestore: Capability[Firestore] = Capability("firestore")
  val loginNotifier: Capability[LoginNotifier] = Capability("login-notifier")
