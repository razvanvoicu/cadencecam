package sgrv.fe

import org.scalajs.dom
import sgrv.api.*
import zio.json.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*

/** The frontend's typed view of the backend API.
  *
  * [[HttpService]] deliberately knows only HTTP and the session-expiry status. Endpoint names, request bodies and JSON
  * shapes live here, so feature controllers cannot accidentally disagree about how the same backend operation works.
  */
private[fe] final class ApiClient(private[fe] val http: HttpService):
  import ApiClient.*

  def session(): Future[MeResult] =
    http.get("/me").flatMap: response =>
      if response.ok then response.text().map(parseUser)
      else if response.status == 401 then Future.successful(MeResult(UserState.Unauthenticated, None))
      else
        Future.successful(
          MeResult(UserState.AuthenticationFailed(s"The authentication check returned ${response.status}."), None)
        )

  def accountSettings(): Future[AccountSettings] =
    getJson[AccountSettings](AccountSettings.Path, "account settings")

  def saveAccountSettings(settings: AccountSettings): Future[Unit] =
    sendJson(AccountSettings.Path, dom.HttpMethod.PUT, settings.toJson, "save account settings")

  def acquirerPresence(): Future[AcquirerPresence] =
    getJson[AcquirerPresence](AcquirerPresence.Path, "counter presence")

  def takeCounterRole(): Future[Unit] =
    sendEmpty(AcquirerPresence.Path, dom.HttpMethod.POST, "take the counting role")

  def workoutHistory(): Future[WorkoutHistory] =
    getJson[WorkoutHistory](WorkoutHistory.Path, "workout history")

  def discardWorkout(id: String): Future[Unit] =
    sendJson(DiscardWorkout.Path, dom.HttpMethod.POST, DiscardWorkout(id).toJson, "delete that workout")

  def deleteAccountData(): Future[Unit] =
    sendEmpty(AccountData.Path, dom.HttpMethod.DELETE, "delete the account's data")

  def logout(): Future[Unit] =
    sendEmpty("/logout", dom.HttpMethod.POST, "log out", includeResponseDetails = true)

  def about(): Future[AboutInfo] =
    getJson[AboutInfo]("/about", "About information")

  private def getJson[A: JsonDecoder](path: String, description: String): Future[A] =
    http.get(path).flatMap(response => decode(response, description))

  private def sendJson(
      path: String,
      requestMethod: dom.HttpMethod,
      requestBody: String,
      description: String
  ): Future[Unit] =
    val init = new dom.RequestInit:
      method = requestMethod
      headers = js.Dictionary("Content-Type" -> "application/json")
      body = requestBody
    http.send(path, init).flatMap(response => expectOk(response, description))

  private def sendEmpty(
      path: String,
      requestMethod: dom.HttpMethod,
      description: String,
      includeResponseDetails: Boolean = false
  ): Future[Unit] =
    val init = new dom.RequestInit:
      method = requestMethod
    http.send(path, init).flatMap: response =>
      if response.ok then Future.successful(())
      else if includeResponseDetails then
        response.text().flatMap: text =>
          val details = Option(text).map(_.trim).filter(_.nonEmpty).getOrElse(s"HTTP ${response.status}")
          Future.failed(RuntimeException(details))
      else expectOk(response, description)

  private def decode[A: JsonDecoder](response: dom.Response, description: String): Future[A] =
    if !response.ok then Future.failed(ApiFailure(description, response.status))
    else
      response.text().flatMap: text =>
        text.fromJson[A].fold(
          details => Future.failed(RuntimeException(s"The backend returned invalid $description JSON: $details")),
          Future.successful
        )

  private def expectOk(response: dom.Response, description: String): Future[Unit] =
    if response.ok then Future.successful(())
    else Future.failed(ApiFailure(description, response.status))

private[fe] object ApiClient:
  /** What `/me` told us: who is signed in, and the identity the backend knows this browser's session by. */
  final case class MeResult(user: UserState, countingSessionId: Option[String])

  final case class ApiFailure(action: String, status: Int)
      extends RuntimeException(s"Could not $action (HTTP $status).")

  /** Reads `/me`: the name comes from the Google account, and falls back to the email address when it has none. */
  private[fe] def parseUser(json: String): MeResult =
    import UserState.*

    json
      .fromJson[CurrentUser]
      .fold(
        details => MeResult(AuthenticationFailed(s"The backend returned invalid user JSON: $details"), None),
        currentUser =>
          val user = Option(currentUser.email)
            .map(_.trim)
            .filter(_.nonEmpty)
            .map: address =>
              SignedIn(address, Option(currentUser.name).map(_.trim).filter(_.nonEmpty).getOrElse(address))
            .getOrElse(AuthenticationFailed("The backend returned no email address."))
          // An unreadable or absent entry is not an authentication problem, so it never downgrades the user state.
          val countingSessionId = currentUser.extra
            .getOrElse(Map.empty)
            .get(CountingSession.Key)
            .flatMap(_.as[CountingSession].toOption)
            .map(_.sessionId)
            .filter(_.nonEmpty)
          MeResult(user, countingSessionId)
      )

private[fe] def errorMessage(error: Throwable): String =
  Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse("The request failed.")
