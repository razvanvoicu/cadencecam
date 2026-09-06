package sgrv.be.auth

import com.google.api.client.googleapis.auth.oauth2.{
  GoogleAuthorizationCodeTokenRequest,
  GoogleIdTokenVerifier,
  GoogleRefreshTokenRequest
}
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.http.{GenericUrl, HttpResponseException, HttpTransport, UrlEncodedContent}
import com.google.api.client.json.gson.GsonFactory
import com.google.gson.JsonParser
import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Collections
import scala.util.Try
import zio.{Task, UIO, ZIO, ZLayer}

final case class SessionUser(
    email: String,
    name: String,
    refreshToken: Option[String] = None,
    accessTokenForRevocation: Option[String] = None
)
final case class GoogleAuthentication(user: SessionUser)

/** ZIO boundary around the Google OAuth client library. */
trait GoogleOAuth:
  def authorizationUrl(state: String): UIO[String]
  def authenticate(code: String): Task[GoogleAuthentication]
  def callbackIsSecure: UIO[Boolean]
  def accessToken(refreshToken: String): Task[String]
  def revoke(refreshToken: String): Task[Unit]

private[be] object GoogleOAuth:
  private val authorizationEndpoint = "https://accounts.google.com/o/oauth2/v2/auth"
  private val baseScopes = Seq("openid", "email", "profile")

  def authorizationUrl(state: String): ZIO[GoogleOAuth, Nothing, String] =
    ZIO.serviceWithZIO[GoogleOAuth](_.authorizationUrl(state))

  def authenticate(code: String): ZIO[GoogleOAuth, Throwable, GoogleAuthentication] =
    ZIO.serviceWithZIO[GoogleOAuth](_.authenticate(code))

  def callbackIsSecure: ZIO[GoogleOAuth, Nothing, Boolean] =
    ZIO.serviceWithZIO[GoogleOAuth](_.callbackIsSecure)

  /** Exchanges a stored OAuth refresh token for a fresh, short-lived access token, used by backend routes acting on
    * Google APIs (e.g. Sheets/Drive) on behalf of a signed-in session.
    */
  def accessToken(refreshToken: String): ZIO[GoogleOAuth, Throwable, String] =
    ZIO.serviceWithZIO[GoogleOAuth](_.accessToken(refreshToken))

  /** Revokes the stored refresh token and its Google OAuth grant. */
  def revoke(refreshToken: String): ZIO[GoogleOAuth, Throwable, Unit] =
    ZIO.serviceWithZIO[GoogleOAuth](_.revoke(refreshToken))

  val live: ZLayer[AppConfig, Throwable, GoogleOAuth] =
    ZLayer.scoped:
      for
        config <- ZIO.service[AppConfig]
        transport <- ZIO.acquireRelease(
          ZIO.attemptBlocking(GoogleNetHttpTransport.newTrustedTransport())
        )(value => ZIO.attemptBlocking(value.shutdown()).ignore)
      yield Live(config.oauth, config.googleServices, transport)

  private final case class Live(config: OAuthConfig, googleServices: Seq[String], transport: HttpTransport)
      extends GoogleOAuth:
    private val jsonFactory = GsonFactory.getDefaultInstance

    override def authorizationUrl(state: String): UIO[String] =
      ZIO.succeed(GoogleOAuth.authorizationUrl(config.clientId, config.callbackUri, state, googleServices))

    override def authenticate(code: String): Task[GoogleAuthentication] =
      ZIO.attemptBlocking:
        val tokenResponse = new GoogleAuthorizationCodeTokenRequest(
          transport,
          jsonFactory,
          config.clientId,
          config.clientSecret,
          code,
          config.callbackUri
        ).execute()
        val verifier = new GoogleIdTokenVerifier.Builder(transport, jsonFactory)
          .setAudience(java.util.List.of(config.clientId))
          .build()
        val idToken = Option(verifier.verify(tokenResponse.getIdToken))
          .getOrElse(throw new IllegalStateException("The Google ID token failed verification"))
        val payload = idToken.getPayload
        val email = Option(payload.getEmail)
          .map(_.trim)
          .filter(_.nonEmpty)
          .getOrElse(throw new IllegalStateException("The Google ID token carries no email address"))
        if payload.getEmailVerified != java.lang.Boolean.TRUE then
          throw new IllegalStateException(s"Google reports $email as unverified")
        val accessToken = Option(tokenResponse.getAccessToken)
          .map(_.trim)
          .filter(_.nonEmpty)
          .getOrElse(throw new IllegalStateException("Google returned no access token"))
        val refreshToken = Option(tokenResponse.getRefreshToken).map(_.trim).filter(_.nonEmpty)
        val accessTokenForRevocation = Option.when(refreshToken.isEmpty)(accessToken)
        val user = SessionUser(
          email,
          displayName(Option(payload.get("name")).map(_.toString), email),
          refreshToken,
          accessTokenForRevocation
        )
        GoogleAuthentication(user)

    override def callbackIsSecure: UIO[Boolean] = ZIO.succeed(config.callbackIsSecure)

    override def accessToken(refreshToken: String): Task[String] =
      ZIO.attemptBlocking:
        val tokenResponse =
          new GoogleRefreshTokenRequest(transport, jsonFactory, refreshToken, config.clientId, config.clientSecret)
            .execute()
        Option(tokenResponse.getAccessToken)
          .map(_.trim)
          .filter(_.nonEmpty)
          .getOrElse(throw new IllegalStateException("Google returned no access token for the stored refresh token"))

    override def revoke(refreshToken: String): Task[Unit] =
      ZIO
        .attemptBlocking:
          val content = new UrlEncodedContent(Collections.singletonMap("token", refreshToken))
          val response = transport
            .createRequestFactory()
            .buildPostRequest(new GenericUrl(GoogleOAuth.revocationEndpoint), content)
            .execute()
          try ()
          finally response.disconnect()
        .catchSome:
          case error: HttpResponseException if GoogleOAuth.isAlreadyRevoked(error) => ZIO.unit

  private[auth] def authorizationUrl(
      clientId: String,
      redirectUri: String,
      state: String,
      extraScopes: Seq[String] = Seq.empty
  ): String =
    Seq(
      "client_id" -> clientId,
      "redirect_uri" -> redirectUri,
      "response_type" -> "code",
      "scope" -> (baseScopes ++ extraScopes).mkString(" "),
      "state" -> state,
      // "consent" forces Google to show the full consent screen (and reissue a refresh token) on every login,
      // rather than silently reusing a prior grant that may predate scopes this app has since started requesting.
      "prompt" -> "select_account consent",
      "access_type" -> "offline"
    ).map((name, value) => s"$name=${URLEncoder.encode(value, UTF_8)}")
      .mkString(s"$authorizationEndpoint?", "&", "")

  private[auth] def displayName(name: Option[String], email: String): String =
    name
      .map(_.trim)
      .filter(_.nonEmpty)
      .getOrElse:
        email.takeWhile(_ != '@') match
          case ""          => email
          case mailboxName => mailboxName

  private val revocationEndpoint = "https://oauth2.googleapis.com/revoke"

  private[auth] def isAlreadyRevoked(error: HttpResponseException): Boolean =
    error.getStatusCode == 400 &&
      Option(error.getContent)
        .flatMap: content =>
          Try(JsonParser.parseString(content).getAsJsonObject.get("error").getAsString).toOption
        .contains("invalid_token")
