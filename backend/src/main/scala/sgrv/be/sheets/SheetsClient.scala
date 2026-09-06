package sgrv.be.sheets

import com.google.gson.{JsonObject, JsonParser}
import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8
import scala.jdk.CollectionConverters.*
import zio.{IO, ZIO}
import zio.http.{Body, Client, Header, Headers, MediaType, Method, Request, Response, URL}

/** ZIO boundary around the Google Sheets v4 and Drive v3 REST APIs, authenticated with a per-call OAuth access token.
  * Used by the `/sheets` routes to act on Google Sheets on behalf of a signed-in session.
  */
private[sheets] trait SheetsClient:
  def findSpreadsheet(accessToken: String, name: String): IO[SheetsError, Option[String]]
  def createSpreadsheet(accessToken: String, name: String): IO[SheetsError, String]
  def appendRow(accessToken: String, spreadsheetId: String, values: Seq[String]): IO[SheetsError, Unit]
  def readColumns(accessToken: String, spreadsheetId: String, range: String): IO[SheetsError, Seq[Seq[String]]]

  final def ensureSpreadsheet(accessToken: String, name: String): IO[SheetsError, String] =
    findSpreadsheet(accessToken, name).flatMap(_.fold(createSpreadsheet(accessToken, name))(ZIO.succeed))

private[sheets] object SheetsClient:
  import SheetsError.*

  /** Constructs the Sheets plugin's private API adapter from the host's generic HTTP client capability. */
  def fromClient(client: Client): SheetsClient = Live(client)

  private final case class Live(client: Client) extends SheetsClient:
    private val driveFilesUrl = "https://www.googleapis.com/drive/v3/files"
    private val sheetsBaseUrl = "https://sheets.googleapis.com/v4/spreadsheets"

    override def findSpreadsheet(accessToken: String, name: String): IO[SheetsError, Option[String]] =
      for
        url <- urlWithQuery(driveFilesUrl, Seq("q" -> driveQuery(name), "fields" -> "files(id)", "pageSize" -> "1"))
        response <- call(Method.GET, url, accessToken, body = None)
        spreadsheetId <- expected(response):
          Option(response.json.getAsJsonArray("files")).toSeq
            .flatMap(_.asScala)
            .headOption
            .map(_.getAsJsonObject.get("id").getAsString)
      yield spreadsheetId

    override def createSpreadsheet(accessToken: String, name: String): IO[SheetsError, String] =
      for
        url <- parseUrl(sheetsBaseUrl)
        properties = new JsonObject
        _ = properties.addProperty("title", name)
        body = new JsonObject
        _ = body.add("properties", properties)
        response <- call(Method.POST, url, accessToken, body = Some(body))
        id <- expected(response)(response.json.get("spreadsheetId").getAsString)
      yield id

    override def appendRow(accessToken: String, spreadsheetId: String, values: Seq[String]): IO[SheetsError, Unit] =
      for
        url <- urlWithQuery(
          s"$sheetsBaseUrl/$spreadsheetId/values/A:B:append",
          Seq("valueInputOption" -> "RAW", "insertDataOption" -> "INSERT_ROWS")
        )
        row = new com.google.gson.JsonArray
        _ = values.foreach(row.add)
        rows = new com.google.gson.JsonArray
        _ = rows.add(row)
        body = new JsonObject
        _ = body.add("values", rows)
        _ <- call(Method.POST, url, accessToken, body = Some(body))
      yield ()

    override def readColumns(
        accessToken: String,
        spreadsheetId: String,
        range: String
    ): IO[SheetsError, Seq[Seq[String]]] =
      for
        url <- parseUrl(s"$sheetsBaseUrl/$spreadsheetId/values/$range")
        response <- call(Method.GET, url, accessToken, body = None)
        values <- expected(response):
          Option(response.json.getAsJsonArray("values")).toSeq
            .flatMap(_.asScala)
            .map(_.getAsJsonArray.asScala.map(_.getAsString).toSeq)
      yield values

    private def driveQuery(name: String): String =
      val escaped = name.replace("\\", "\\\\").replace("'", "\\'")
      s"mimeType='application/vnd.google-apps.spreadsheet' and trashed=false and name='$escaped'"

    private def parseUrl(raw: String): IO[SheetsError, URL] =
      URL.decode(raw) match
        case Right(url)  => ZIO.succeed(url)
        case Left(error) =>
          ZIO.fail(
            UnexpectedGoogleResponse(
              "constructing a Google API request",
              None,
              None,
              Some(new IllegalArgumentException(s"Invalid Google API URL $raw: $error"))
            )
          )

    private def urlWithQuery(base: String, params: Seq[(String, String)]): IO[SheetsError, URL] =
      val query = params.map((key, value) => s"$key=${URLEncoder.encode(value, UTF_8)}").mkString("&")
      parseUrl(s"$base?$query")

    private final case class GoogleResponse(operation: String, statusCode: Int, text: String, json: JsonObject)

    private def call(
        method: Method,
        url: URL,
        accessToken: String,
        body: Option[JsonObject]
    ): IO[SheetsError, GoogleResponse] =
      val operation = s"$method $url"
      for
        response <- client
          .batched(
            Request(
              method = method,
              url = url,
              headers = requestHeaders(accessToken, body.isDefined),
              body = body.fold(Body.empty)(json => Body.fromString(json.toString))
            )
          )
          .mapError(error => GoogleUnavailable(operation, cause = Some(error)))
        text <- response.body.asString.mapError(error => GoogleUnavailable(operation, cause = Some(error)))
        _ <- failIfUnsuccessful(operation, response, text)
        json <- ZIO
          .attempt(JsonParser.parseString(if text.isBlank then "{}" else text).getAsJsonObject)
          .mapError(error => UnexpectedGoogleResponse(operation, Some(response.status.code), Some(text), Some(error)))
      yield GoogleResponse(operation, response.status.code, text, json)

    private def requestHeaders(accessToken: String, hasBody: Boolean): Headers =
      val authorization = Headers(Header.Authorization.Bearer(accessToken))
      if hasBody then authorization ++ Headers(Header.ContentType(MediaType.application.json)) else authorization

    private def failIfUnsuccessful(operation: String, response: Response, text: String): IO[SheetsError, Unit] =
      if response.status.isSuccess then ZIO.unit
      else ZIO.fail(unsuccessfulResponse(operation, response.status.code, text))

    private def expected[A](response: GoogleResponse)(value: => A): IO[SheetsError, A] =
      ZIO
        .attempt(value)
        .mapError(error =>
          UnexpectedGoogleResponse(
            response.operation,
            Some(response.statusCode),
            Some(response.text),
            Some(error)
          )
        )

  private[sheets] def unsuccessfulResponse(operation: String, statusCode: Int, text: String): SheetsError =
    statusCode match
      case 401 | 403           => Unauthenticated(Some(s"HTTP $statusCode: $text"))
      case 429                 => GoogleUnavailable(operation, Some(s"HTTP $statusCode: $text"))
      case code if code >= 500 => GoogleUnavailable(operation, Some(s"HTTP $statusCode: $text"))
      case _                   => UnexpectedGoogleResponse(operation, Some(statusCode), Some(text))
