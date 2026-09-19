package sgrv.fe

import org.scalajs.dom
import sgrv.api.*
import zio.json.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*
import scala.util.control.NonFatal

/** The frontend's typed view of the ordinary request/response backend API.
  *
  * [[HttpService]] deliberately knows only `fetch` and the session-expiry status. Endpoint names, request bodies,
  * success criteria, bounded error bodies, JSON shapes and retry decisions live here, so feature controllers receive a
  * value or an [[ApiClient.ApiError]] rather than reconstructing HTTP policy themselves.
  *
  * Peer signaling, progress reporting and trace capture remain protocol-specific helpers over [[HttpService]]: their
  * polling, displacement and upload state machines are not ordinary one-shot API calls and already own their lifecycle.
  */
private[fe] final class ApiClient(private[fe] val http: HttpService):
  import ApiClient.*

  def session(): ApiCall[MeResult] =
    getJson[CurrentUser]("/me", "check authentication", "signed-in user", RetryPolicy.Never)
      .map:
        case Right(currentUser) => userResult(currentUser)
        // `/me` uses 401 to say there is no session. That is an ordinary anonymous state, not a failed request.
        case Left(ApiError.Http(_, 401, _)) => Right(MeResult(UserState.Unauthenticated, None))
        case Left(error)                    => Left(error)

  def accountSettings(): ApiCall[AccountSettings] =
    getJson[AccountSettings](AccountSettings.Path, "read account settings", "account settings")

  def saveAccountSettings(settings: AccountSettings): ApiCall[Unit] =
    sendJson(AccountSettings.Path, dom.HttpMethod.PUT, settings.toJson, "save account settings")

  def acquirerPresence(): ApiCall[AcquirerPresence] =
    getJson[AcquirerPresence](AcquirerPresence.Path, "read counter presence", "counter presence")

  def takeCounterRole(): ApiCall[Unit] =
    sendEmpty(AcquirerPresence.Path, dom.HttpMethod.POST, "take the counting role")

  def workoutHistory(): ApiCall[WorkoutHistory] =
    getJson[WorkoutHistory](WorkoutHistory.Path, "read workout history", "workout history")

  def discardWorkout(id: String): ApiCall[Unit] =
    sendJson(DiscardWorkout.Path, dom.HttpMethod.POST, DiscardWorkout(id).toJson, "delete that workout")

  def deleteAccountData(): ApiCall[Unit] =
    sendEmpty(AccountData.Path, dom.HttpMethod.DELETE, "delete the account's data")

  def logout(): ApiCall[Unit] =
    sendEmpty("/logout", dom.HttpMethod.POST, "log out")

  def about(): ApiCall[AboutInfo] =
    getJson[AboutInfo]("/about", "read About information", "About information")

  private def getJson[A: JsonDecoder](
      path: String,
      action: String,
      description: String,
      retry: RetryPolicy = RetryPolicy.IdempotentRead
  ): ApiCall[A] =
    request(() => http.get(path), action, retry)(text => decodeJson[A](text, description))

  private def sendJson(
      path: String,
      requestMethod: dom.HttpMethod,
      requestBody: String,
      action: String
  ): ApiCall[Unit] =
    val init = new dom.RequestInit:
      method = requestMethod
      headers = js.Dictionary("Content-Type" -> "application/json")
      body = requestBody
    request(() => http.send(path, init), action, RetryPolicy.Never)(_ => Right(()))

  private def sendEmpty(path: String, requestMethod: dom.HttpMethod, action: String): ApiCall[Unit] =
    val init = new dom.RequestInit:
      method = requestMethod
    request(() => http.send(path, init), action, RetryPolicy.Never)(_ => Right(()))

  /** Runs one request under the endpoint's retry policy and turns every ordinary failure into the value channel.
    *
    * Only idempotent reads opt into retry, once, for a transport failure or the handful of statuses that explicitly
    * describe a temporary condition. Mutations never retry: whether the server applied a request before the connection
    * failed is unknowable, so replaying one could delete or create twice.
    */
  private def request[A](
      fetch: () => Future[dom.Response],
      action: String,
      retry: RetryPolicy
  )(decode: String => ApiResult[A]): ApiCall[A] =
    def attempt(number: Int): ApiCall[A] =
      fetch()
        .flatMap: response =>
          if retry.shouldRetryStatus(response.status, number) then after(retry.delayMillis)(attempt(number + 1))
          else
            response
              .text()
              .map: body =>
                if response.ok then decode(body)
                else Left(ApiError.Http(action, response.status, errorBody(body)))
        .recoverWith:
          case NonFatal(_) if retry.shouldRetryTransport(number) => after(retry.delayMillis)(attempt(number + 1))
          case NonFatal(error)                                   =>
            Future.successful(Left(ApiError.Transport(action, throwableMessage(error))))

    attempt(0)

  private def after[A](delayMillis: Int)(next: => Future[A]): Future[A] =
    val result = Promise[A]()
    val _ = dom.window.setTimeout(
      () => next.onComplete(result.complete),
      delayMillis.toDouble
    )
    result.future

private[fe] object ApiClient:
  type ApiResult[+A] = Either[ApiError, A]
  type ApiCall[+A] = Future[ApiResult[A]]

  /** A failure callers can render or deliberately replace with feature-specific wording. */
  enum ApiError:
    case Http(action: String, status: Int, body: Option[String])
    case Decode(description: String, details: String)
    case Transport(action: String, details: String)

    def message: String = this match
      case Http(action, status, Some(body)) => s"Could not $action (HTTP $status): $body"
      case Http(action, status, None)       => s"Could not $action (HTTP $status)."
      case Decode(description, details)     => s"The backend returned invalid $description JSON: $details"
      case Transport(action, details)       => s"Could not $action: $details"

  /** Retry is deliberately a property of an endpoint, not a caller reaction to a generic failure. */
  final case class RetryPolicy(maxRetries: Int, delayMillis: Int, retryableStatuses: Set[Int]):
    def shouldRetryStatus(status: Int, attemptsAlreadyMade: Int): Boolean =
      attemptsAlreadyMade < maxRetries && retryableStatuses.contains(status)

    def shouldRetryTransport(attemptsAlreadyMade: Int): Boolean =
      attemptsAlreadyMade < maxRetries

  object RetryPolicy:
    val Never: RetryPolicy = RetryPolicy(maxRetries = 0, delayMillis = 0, retryableStatuses = Set.empty)

    // One short retry absorbs a dropped mobile request or a transient proxy response without hiding a persistent fault.
    val IdempotentRead: RetryPolicy =
      RetryPolicy(maxRetries = 1, delayMillis = 250, retryableStatuses = Set(429, 502, 503, 504))

  /** What `/me` told us: who is signed in, and the identity the backend knows this browser's session by. */
  final case class MeResult(user: UserState, countingSessionId: Option[String])

  private val MaximumErrorBodyCharacters = 400

  private[fe] def decodeJson[A: JsonDecoder](json: String, description: String): ApiResult[A] =
    json.fromJson[A].left.map(details => ApiError.Decode(description, details))

  /** Reads `/me`: the name comes from the Google account, and falls back to the email address when it has none. */
  private[fe] def parseUser(json: String): ApiResult[MeResult] =
    decodeJson[CurrentUser](json, "signed-in user").flatMap(userResult)

  private def userResult(currentUser: CurrentUser): ApiResult[MeResult] =
    import UserState.*

    Option(currentUser.email)
      .map(_.trim)
      .filter(_.nonEmpty)
      .toRight(ApiError.Decode("signed-in user", "the response contained no email address"))
      .map: address =>
        val user = SignedIn(address, Option(currentUser.name).map(_.trim).filter(_.nonEmpty).getOrElse(address))
        // An unreadable or absent entry is not an authentication problem, so it never downgrades the user state.
        val countingSessionId = currentUser.extra
          .getOrElse(Map.empty)
          .get(CountingSession.Key)
          .flatMap(_.as[CountingSession].toOption)
          .map(_.sessionId)
          .filter(_.nonEmpty)
        MeResult(user, countingSessionId)

  private[fe] def errorBody(body: String): Option[String] =
    Option(body)
      .map(_.trim)
      .filter(_.nonEmpty)
      .map: value =>
        if value.length <= MaximumErrorBodyCharacters then value
        else value.take(MaximumErrorBodyCharacters) + "…"

  private def throwableMessage(error: Throwable): String =
    Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse("The request failed.")

private[fe] def errorMessage(error: Throwable): String =
  Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse("The request failed.")
