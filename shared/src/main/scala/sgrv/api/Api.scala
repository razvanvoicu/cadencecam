package sgrv.api

import zio.json.{DeriveJsonCodec, JsonCodec, jsonNoExtraFields}

@jsonNoExtraFields
final case class CurrentUser(email: String, name: String)

object CurrentUser:
  given JsonCodec[CurrentUser] = DeriveJsonCodec.gen[CurrentUser]

@jsonNoExtraFields
final case class AboutInfo(
    appVersion: String,
    buildDate: String,
    buildOs: String,
    scalaVersion: String,
    scalaJsVersion: String
)

object AboutInfo:
  given JsonCodec[AboutInfo] = DeriveJsonCodec.gen[AboutInfo]

@jsonNoExtraFields
final case class UpsertSpreadsheetRequest(name: String)

object UpsertSpreadsheetRequest:
  given JsonCodec[UpsertSpreadsheetRequest] = DeriveJsonCodec.gen[UpsertSpreadsheetRequest]

@jsonNoExtraFields
final case class UpsertSpreadsheetResponse(spreadsheetId: String)

object UpsertSpreadsheetResponse:
  given JsonCodec[UpsertSpreadsheetResponse] = DeriveJsonCodec.gen[UpsertSpreadsheetResponse]

@jsonNoExtraFields
final case class SpreadsheetContentResponse(rows: Seq[Seq[String]])

object SpreadsheetContentResponse:
  given JsonCodec[SpreadsheetContentResponse] = DeriveJsonCodec.gen[SpreadsheetContentResponse]
