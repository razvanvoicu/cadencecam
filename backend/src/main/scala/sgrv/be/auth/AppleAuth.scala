package sgrv.be.auth

import com.google.gson.JsonParser
import com.nimbusds.jose.{JWSAlgorithm, JWSHeader}
import com.nimbusds.jose.crypto.{ECDSASigner, RSASSAVerifier}
import com.nimbusds.jose.jwk.{JWKSet, RSAKey}
import com.nimbusds.jwt.{JWTClaimsSet, SignedJWT}
import java.net.{URI, URLEncoder}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets.UTF_8
import java.security.KeyFactory
import java.security.interfaces.ECPrivateKey
import java.security.spec.PKCS8EncodedKeySpec
import java.time.{Duration, Instant}
import java.util.{Base64, Date}
import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters.*
import zio.{Task, UIO, ZIO, ZLayer}

final case class AppleAuthentication(user: SessionUser)

/** Server-side half of Sign in with Apple.
  *
  * Native and browser clients submit Apple's five-minute, single-use authorization code. Only this service exchanges
  * it, verifies the returned identity token against Apple's rotating public keys, and retains the refresh token. The
  * app itself receives only CadenceCam's opaque session token.
  */
trait AppleOAuth:
  def configured: UIO[Boolean]
  def authorizationUrl(state: String): Task[String]
  def authenticateBrowser(
      code: String,
      givenName: Option[String],
      familyName: Option[String]
  ): Task[AppleAuthentication]
  def authenticateNative(code: String, givenName: Option[String], familyName: Option[String]): Task[AppleAuthentication]
  def validate(refreshToken: String, clientId: String): Task[Unit]
  def revoke(token: String, clientId: String): Task[Unit]
  def callbackIsSecure: UIO[Boolean]

private[be] object AppleOAuth:
  private val authorizationEndpoint = "https://appleid.apple.com/auth/authorize"
  private val tokenEndpoint = URI.create("https://appleid.apple.com/auth/token")
  private val revocationEndpoint = URI.create("https://appleid.apple.com/auth/revoke")
  private val keysEndpoint = URI.create("https://appleid.apple.com/auth/keys")
  private val issuer = "https://appleid.apple.com"

  def configured: ZIO[AppleOAuth, Nothing, Boolean] = ZIO.serviceWithZIO[AppleOAuth](_.configured)
  def authorizationUrl(state: String): ZIO[AppleOAuth, Throwable, String] =
    ZIO.serviceWithZIO[AppleOAuth](_.authorizationUrl(state))
  def authenticateBrowser(
      code: String,
      givenName: Option[String],
      familyName: Option[String]
  ): ZIO[AppleOAuth, Throwable, AppleAuthentication] =
    ZIO.serviceWithZIO[AppleOAuth](_.authenticateBrowser(code, givenName, familyName))
  def authenticateNative(
      code: String,
      givenName: Option[String],
      familyName: Option[String]
  ): ZIO[AppleOAuth, Throwable, AppleAuthentication] =
    ZIO.serviceWithZIO[AppleOAuth](_.authenticateNative(code, givenName, familyName))
  def validate(refreshToken: String, clientId: String): ZIO[AppleOAuth, Throwable, Unit] =
    ZIO.serviceWithZIO[AppleOAuth](_.validate(refreshToken, clientId))
  def revoke(token: String, clientId: String): ZIO[AppleOAuth, Throwable, Unit] =
    ZIO.serviceWithZIO[AppleOAuth](_.revoke(token, clientId))
  def callbackIsSecure: ZIO[AppleOAuth, Nothing, Boolean] =
    ZIO.serviceWithZIO[AppleOAuth](_.callbackIsSecure)

  val live: ZLayer[AppConfig, Nothing, AppleOAuth] = ZLayer.fromFunction: (app: AppConfig) =>
    Live(app.appleOAuth, HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build())

  private final case class TokenResponse(idToken: String, refreshToken: Option[String], accessToken: String)

  private final class Live(config: Option[AppleOAuthConfig], http: HttpClient) extends AppleOAuth:
    private val cachedKeys = AtomicReference(Option.empty[(Instant, JWKSet)])

    override def configured: UIO[Boolean] = ZIO.succeed(config.nonEmpty)

    override def authorizationUrl(state: String): Task[String] =
      requireConfig.map: apple =>
        query(
          authorizationEndpoint,
          Seq(
            "client_id" -> apple.serviceId,
            "redirect_uri" -> apple.callbackUri,
            "response_type" -> "code",
            "response_mode" -> "form_post",
            "scope" -> "name email",
            "state" -> state
          )
        )

    override def authenticateBrowser(
        code: String,
        givenName: Option[String],
        familyName: Option[String]
    ): Task[AppleAuthentication] =
      requireConfig.flatMap(apple =>
        authenticate(code, apple.serviceId, Some(apple.callbackUri), givenName, familyName)
      )

    override def authenticateNative(
        code: String,
        givenName: Option[String],
        familyName: Option[String]
    ): Task[AppleAuthentication] =
      requireConfig.flatMap(apple => authenticate(code, apple.iosClientId, None, givenName, familyName))

    private def authenticate(
        code: String,
        clientId: String,
        redirectUri: Option[String],
        givenName: Option[String],
        familyName: Option[String]
    ): Task[AppleAuthentication] =
      for
        submitted <- bounded(code, "The Apple authorization code is missing or too large")
        response <- tokenRequest(
          clientId,
          Seq("code" -> submitted, "grant_type" -> "authorization_code") ++ redirectUri.map("redirect_uri" -> _)
        )
        claims <- verifyIdentityToken(response.idToken, clientId)
        subject <- required(Option(claims.getSubject), "Apple returned no user identifier")
        refreshToken <- required(response.refreshToken, "Apple returned no refresh token")
        email = normalized(Option(claims.getStringClaim("email"))).getOrElse(s"Apple account ${subject.take(8)}")
        suppliedName = Seq(givenName, familyName).flatten.map(_.trim).filter(_.nonEmpty).mkString(" ")
        name = Option(suppliedName).filter(_.nonEmpty).getOrElse(GoogleOAuth.displayName(None, email))
      yield AppleAuthentication(
        SessionUser(
          email = email,
          name = name,
          refreshToken = Some(refreshToken),
          identityProvider = IdentityProvider.Apple,
          providerSubject = Some(subject),
          accountId = Some(s"apple:$subject"),
          providerClientId = Some(clientId)
        )
      )

    override def validate(refreshToken: String, clientId: String): Task[Unit] =
      tokenRequest(clientId, Seq("refresh_token" -> refreshToken, "grant_type" -> "refresh_token")).unit

    override def revoke(token: String, clientId: String): Task[Unit] =
      requireConfig.flatMap: apple =>
        for
          secret <- clientSecret(apple, clientId)
          _ <- sendForm(
            revocationEndpoint,
            Seq(
              "client_id" -> clientId,
              "client_secret" -> secret,
              "token" -> token,
              "token_type_hint" -> "refresh_token"
            )
          )
        yield ()

    override def callbackIsSecure: UIO[Boolean] =
      ZIO.succeed(config.exists(_.publicBaseUrl.startsWith("https://")))

    private def tokenRequest(clientId: String, parameters: Seq[(String, String)]): Task[TokenResponse] =
      requireConfig.flatMap: apple =>
        for
          secret <- clientSecret(apple, clientId)
          body <- sendForm(
            tokenEndpoint,
            Seq("client_id" -> clientId, "client_secret" -> secret) ++ parameters
          )
          parsed <- ZIO.attempt:
            val json = JsonParser.parseString(body).getAsJsonObject
            TokenResponse(
              requiredJson(json, "id_token"),
              Option(json.get("refresh_token")).map(_.getAsString).flatMap(value => normalized(Option(value))),
              requiredJson(json, "access_token")
            )
        yield parsed

    private def sendForm(endpoint: URI, parameters: Seq[(String, String)]): Task[String] =
      ZIO.attemptBlocking:
        val body = parameters.map((name, value) => s"${encode(name)}=${encode(value)}").mkString("&")
        val request = HttpRequest
          .newBuilder(endpoint)
          .timeout(Duration.ofSeconds(20))
          .header("Content-Type", "application/x-www-form-urlencoded")
          .POST(HttpRequest.BodyPublishers.ofString(body))
          .build()
        val response = http.send(request, HttpResponse.BodyHandlers.ofString())
        if response.statusCode() / 100 != 2 then
          throw new IllegalStateException(s"Apple token endpoint returned HTTP ${response.statusCode()}")
        response.body()

    private def clientSecret(apple: AppleOAuthConfig, clientId: String): Task[String] = ZIO.attempt:
      val now = Instant.now()
      val claims = new JWTClaimsSet.Builder()
        .issuer(apple.teamId)
        .subject(clientId)
        .audience(issuer)
        .issueTime(Date.from(now))
        .expirationTime(Date.from(now.plus(Duration.ofDays(30))))
        .build()
      val jwt = SignedJWT(new JWSHeader.Builder(JWSAlgorithm.ES256).keyID(apple.keyId).build(), claims)
      jwt.sign(ECDSASigner(privateKey(apple.privateKeyPem)))
      jwt.serialize()

    private def verifyIdentityToken(encoded: String, clientId: String): Task[JWTClaimsSet] = ZIO.attemptBlocking:
      val jwt = SignedJWT.parse(encoded)
      if jwt.getHeader.getAlgorithm != JWSAlgorithm.RS256 then
        throw new IllegalArgumentException("Apple identity token uses an unexpected signature algorithm")
      val keyId = Option(jwt.getHeader.getKeyID)
        .getOrElse(throw new IllegalArgumentException("Apple identity token has no key id"))
      val verified = verify(jwt, keyId, appleKeys(force = false)) || verify(jwt, keyId, appleKeys(force = true))
      if !verified then throw new IllegalArgumentException("Apple identity token signature is invalid")
      val claims = jwt.getJWTClaimsSet
      val now = Instant.now()
      if claims.getIssuer != issuer then throw new IllegalArgumentException("Apple identity token issuer is invalid")
      if !claims.getAudience.asScala.contains(clientId) then
        throw new IllegalArgumentException("Apple identity token audience is invalid")
      if Option(claims.getExpirationTime).forall(_.toInstant.isBefore(now.minusSeconds(60))) then
        throw new IllegalArgumentException("Apple identity token is expired")
      if Option(claims.getIssueTime).exists(_.toInstant.isAfter(now.plusSeconds(60))) then
        throw new IllegalArgumentException("Apple identity token issue time is invalid")
      claims

    private def verify(jwt: SignedJWT, keyId: String, keys: JWKSet): Boolean =
      Option(keys.getKeyByKeyId(keyId))
        .collect { case key: RSAKey => key }
        .exists: key =>
          jwt.verify(RSASSAVerifier(key.toRSAPublicKey))

    private def appleKeys(force: Boolean): JWKSet =
      val now = Instant.now()
      cachedKeys.get() match
        case Some((loadedAt, keys)) if !force && loadedAt.plus(Duration.ofHours(1)).isAfter(now) => keys
        case _                                                                                   =>
          val request = HttpRequest.newBuilder(keysEndpoint).timeout(Duration.ofSeconds(20)).GET().build()
          val response = http.send(request, HttpResponse.BodyHandlers.ofString())
          if response.statusCode() / 100 != 2 then
            throw new IllegalStateException(s"Apple key endpoint returned HTTP ${response.statusCode()}")
          val keys = JWKSet.parse(response.body())
          cachedKeys.set(Some(now -> keys))
          keys

    private def requireConfig: Task[AppleOAuthConfig] =
      ZIO.fromOption(config).orElseFail(new IllegalStateException("Sign in with Apple is not configured"))

  private def privateKey(pem: String): ECPrivateKey =
    val encoded = pem
      .replace("-----BEGIN PRIVATE KEY-----", "")
      .replace("-----END PRIVATE KEY-----", "")
      .replaceAll("\\s", "")
    KeyFactory
      .getInstance("EC")
      .generatePrivate(PKCS8EncodedKeySpec(Base64.getDecoder.decode(encoded)))
      .asInstanceOf[ECPrivateKey]

  private def query(endpoint: String, parameters: Seq[(String, String)]): String =
    parameters.map((name, value) => s"${encode(name)}=${encode(value)}").mkString(s"$endpoint?", "&", "")

  private def encode(value: String): String = URLEncoder.encode(value, UTF_8)

  private def bounded(value: String, message: String): Task[String] =
    ZIO
      .fromOption(normalized(Option(value)).filter(_.length <= 16384))
      .orElseFail(new IllegalArgumentException(message))

  private def required(value: Option[String], message: String): Task[String] =
    ZIO.fromOption(value.flatMap(item => normalized(Option(item)))).orElseFail(new IllegalStateException(message))

  private def requiredJson(json: com.google.gson.JsonObject, name: String): String =
    Option(json.get(name))
      .map(_.getAsString)
      .flatMap(value => normalized(Option(value)))
      .getOrElse(throw new IllegalStateException(s"Apple returned no $name"))

  private def normalized(value: Option[String]): Option[String] = value.map(_.trim).filter(_.nonEmpty)
