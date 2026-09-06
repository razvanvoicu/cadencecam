package sgrv.be.sheets

import sgrv.be.BackendCapabilities
import sgrv.be.auth.{GoogleOAuth, SessionStore, SessionUser}
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import sgrv.api.{SpreadsheetContentResponse, UpsertSpreadsheetRequest, UpsertSpreadsheetResponse}
import zio.{Cause, Clock, IO, UIO, ZIO}
import zio.http.{Client, Header, Method, Request, Response, Routes, Status, handler}
import zio.json.*

/** Shared plumbing for the `/sheets` routes. */
private[sheets] object SheetsRoutes:
  import SheetsError.*

  def requireRefreshToken(user: SessionUser): IO[SheetsError, String] =
    ZIO.fromOption(user.refreshToken).orElseFail(Unauthenticated())

  /** `Authenticated` is guaranteed by these plugins' access policy; a different context indicates a host bug. */
  def authenticatedRequest: ZIO[RequestContext, Nothing, RequestContext.Authenticated] =
    ZIO
      .service[RequestContext]
      .flatMap:
        case request: RequestContext.Authenticated => ZIO.succeed(request)
        case _: RequestContext.Public => ZIO.dieMessage("Authenticated route received a public request context")

  def responseFor(error: SheetsError): Response =
    error match
      case InvalidInput(message, _) => Response.text(message).status(Status.BadRequest)
      case _: Unauthenticated       =>
        Response
          .text("Google authorization is missing or expired; sign out and sign in again.")
          .status(Status.Forbidden)
      case _: GoogleUnavailable =>
        Response.text("Google services are temporarily unavailable.").status(Status.ServiceUnavailable)
      case _: UnexpectedGoogleResponse =>
        Response.text("Google returned an unexpected response.").status(Status.BadGateway)

  def handle(error: SheetsError): UIO[Response] =
    val log = error.cause match
      case Some(cause) => ZIO.logErrorCause(error.diagnostic, Cause.fail(cause))
      case None        => ZIO.logError(error.diagnostic)
    log.as(responseFor(error))

  def decodeName(body: String): Either[SheetsError, String] =
    body
      .fromJson[UpsertSpreadsheetRequest]
      .left
      .map(details =>
        InvalidInput(
          "Request body must be valid JSON with a string \"name\" field.",
          Some(new IllegalArgumentException(details))
        )
      )
      .flatMap(request =>
        Option(request.name)
          .map(_.trim)
          .filter(_.nonEmpty)
          .toRight(InvalidInput("Missing or empty \"name\" in the request body"))
      )

/** Finds or creates a spreadsheet by name in the signed-in user's Google Drive, then appends one row recording this
  * request's server timestamp and the requesting browser's User-Agent.
  */
object UpsertSpreadsheet extends BackendPlugin:
  type Requires = GoogleOAuth & SessionStore & Client

  override val id = "sheets-upsert"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.googleOAuth) ++
      CapabilitySet.one(BackendCapabilities.sessionStore) ++
      CapabilitySet.one(BackendCapabilities.httpClient)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.POST / "sheets" / "upsert" -> handler(SheetsRoutes.authenticatedRequest.flatMap(apply)))

  import SheetsRoutes.*

  private def apply(authenticated: RequestContext.Authenticated): ZIO[Requires, Nothing, Response] =
    val request = authenticated.request
    val result =
      for
        refreshToken <- requireRefreshToken(authenticated.user)
        name <- requireName(request)
        accessToken <- GoogleOAuth.accessToken(refreshToken).mapError(SheetsError.fromAccessTokenFailure)
        httpClient <- ZIO.service[Client]
        sheetsClient = SheetsClient.fromClient(httpClient)
        spreadsheetId <- sheetsClient.ensureSpreadsheet(accessToken, name)
        userAgent = request.header(Header.UserAgent).map(_.renderedValue).getOrElse("unknown")
        now <- Clock.instant
        _ <- sheetsClient.appendRow(accessToken, spreadsheetId, Seq(now.toString, userAgent))
      yield Response.json(UpsertSpreadsheetResponse(spreadsheetId).toJson)
    result.foldZIO(handle, ZIO.succeed)

  private def requireName(request: Request): IO[SheetsError, String] =
    for
      body <- request.body.asString.mapError(error =>
        SheetsError.InvalidInput("The request body could not be read.", Some(error))
      )
      name <- ZIO.fromEither(decodeName(body))
    yield name

/** Reports the current content of columns A and B of a spreadsheet by name, or an empty result if no spreadsheet with
  * that name exists yet for the signed-in user.
  */
object SpreadsheetContent extends BackendPlugin:
  type Requires = GoogleOAuth & SessionStore & Client

  override val id = "sheets-content"
  override val requirements: CapabilitySet[Requires] =
    CapabilitySet.one(BackendCapabilities.googleOAuth) ++
      CapabilitySet.one(BackendCapabilities.sessionStore) ++
      CapabilitySet.one(BackendCapabilities.httpClient)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.GET / "sheets" / "content" -> handler(SheetsRoutes.authenticatedRequest.flatMap(apply)))

  import SheetsRoutes.*

  private def apply(authenticated: RequestContext.Authenticated): ZIO[Requires, Nothing, Response] =
    val request = authenticated.request
    val result =
      for
        refreshToken <- requireRefreshToken(authenticated.user)
        name <- ZIO
          .fromOption(request.queryParam("name").map(_.trim).filter(_.nonEmpty))
          .orElseFail(SheetsError.InvalidInput("Missing \"name\" query parameter"))
        accessToken <- GoogleOAuth.accessToken(refreshToken).mapError(SheetsError.fromAccessTokenFailure)
        httpClient <- ZIO.service[Client]
        sheetsClient = SheetsClient.fromClient(httpClient)
        spreadsheetId <- sheetsClient.findSpreadsheet(accessToken, name)
        rows <- spreadsheetId.fold(ZIO.succeed(Seq.empty[Seq[String]])): id =>
          sheetsClient.readColumns(accessToken, id, "A:B")
      yield Response.json(SpreadsheetContentResponse(rows).toJson)
    result.foldZIO(handle, ZIO.succeed)
