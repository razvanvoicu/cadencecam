package sgrv.be

import com.google.cloud.firestore.Firestore
import sgrv.be.auth.{GoogleOAuth, SessionStore, TokenGenerator}
import sgrv.be.core.{Capability, CurrentUserDetails, LoginNotifier}
import zio.http.Client

/** Services the host itself constructs. The services derived from discovered modules — [[sgrv.be.core.LoginNotifier]]
  * and [[sgrv.be.core.CurrentUserDetails]] — are deliberately absent: they are built from this environment at startup
  * and join the capability registry afterwards, so they cannot also be inputs to it.
  */
type BackendEnvironment = GoogleOAuth & SessionStore & TokenGenerator & Client & Firestore

private[be] object BackendCapabilities:
  val googleOAuth: Capability[GoogleOAuth] = Capability("google-oauth")
  val sessionStore: Capability[SessionStore] = Capability("session-store")
  val tokenGenerator: Capability[TokenGenerator] = Capability("token-generator")
  val httpClient: Capability[Client] = Capability("http-client")
  val firestore: Capability[Firestore] = Capability("firestore")
  val loginNotifier: Capability[LoginNotifier] = Capability("login-notifier")
  val currentUserDetails: Capability[CurrentUserDetails] = Capability("current-user-details")
