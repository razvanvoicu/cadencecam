package sgrv.be.sheets

import com.google.api.client.auth.oauth2.TokenResponseException

private[sheets] sealed trait SheetsError:
  def diagnostic: String
  def cause: Option[Throwable]

private[sheets] object SheetsError:
  final case class InvalidInput(clientMessage: String, cause: Option[Throwable] = None) extends SheetsError:
    override val diagnostic: String = clientMessage

  final case class Unauthenticated(
      details: Option[String] = None,
      cause: Option[Throwable] = None
  ) extends SheetsError:
    override val diagnostic: String =
      s"Google authorization is missing or invalid${details.fold("")(value => s": $value")}"

  final case class GoogleUnavailable(
      operation: String,
      details: Option[String] = None,
      cause: Option[Throwable] = None
  ) extends SheetsError:
    override val diagnostic: String =
      s"Google service unavailable during $operation${details.fold("")(value => s": $value")}"

  final case class UnexpectedGoogleResponse(
      operation: String,
      statusCode: Option[Int],
      responseBody: Option[String],
      cause: Option[Throwable] = None
  ) extends SheetsError:
    override val diagnostic: String =
      val status = statusCode.fold("")(value => s" (HTTP $value)")
      val body = responseBody.filter(_.nonEmpty).fold("")(value => s": $value")
      s"Unexpected Google response during $operation$status$body"

  def fromAccessTokenFailure(error: Throwable): SheetsError =
    error match
      case response: TokenResponseException if Set(400, 401, 403).contains(response.getStatusCode) =>
        Unauthenticated(cause = Some(response))
      case response: TokenResponseException if response.getStatusCode == 429 || response.getStatusCode >= 500 =>
        GoogleUnavailable("refreshing the access token", cause = Some(response))
      case error: java.io.IOException =>
        GoogleUnavailable("refreshing the access token", cause = Some(error))
      case error =>
        UnexpectedGoogleResponse("refreshing the access token", None, None, Some(error))
