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
