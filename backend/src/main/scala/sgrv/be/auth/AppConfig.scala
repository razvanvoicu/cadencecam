package sgrv.be.auth

import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import zio.{System, Task, ZIO, ZLayer}

private[be] final case class OAuthConfig(clientId: String, clientSecret: String, publicBaseUrl: String):
  val callbackUri: String = s"$publicBaseUrl/auth/callback"
  val callbackIsSecure: Boolean = publicBaseUrl.startsWith("https://")
private[be] final case class FirestoreConfig(projectId: String, databaseId: String)
private[be] final case class AppleOAuthConfig(
    teamId: String,
    keyId: String,
    privateKeyPem: String,
    iosClientId: String,
    serviceId: String,
    publicBaseUrl: String
):
  val callbackUri: String = s"$publicBaseUrl/auth/apple/callback"

private[be] final case class AppConfig(
    oauth: OAuthConfig,
    firestore: FirestoreConfig,
    googleServices: Seq[String] = Seq.empty,
    appleOAuth: Option[AppleOAuthConfig] = None
)

private[be] object AppConfig:
  def load: Task[AppConfig] = System.envs.flatMap(environment => ZIO.fromEither(fromEnvironment(environment)))

  val live: ZLayer[Any, Throwable, AppConfig] = ZLayer.fromZIO(load)

  private[auth] def fromEnvironment(environment: Map[String, String]): Either[IllegalArgumentException, AppConfig] =
    def required(name: String): Either[IllegalArgumentException, String] =
      environment
        .get(name)
        .map(_.trim)
        .filter(_.nonEmpty)
        .toRight(new IllegalArgumentException(s"Environment variable $name is not set or is empty; see prod.env"))

    for
      clientId <- required("GOOGLE_OAUTH_CLIENT_ID")
      clientSecret <- required("GOOGLE_OAUTH_CLIENT_SECRET")
      configuredBaseUrl <- required("PUBLIC_BASE_URL")
      publicBaseUrl <- validatePublicBaseUrl(configuredBaseUrl)
      projectId <- required("GCP_PROJECT_ID")
      databaseId <- required("FIRESTORE_DATABASE_ID")
      apple <- appleOAuth(environment, publicBaseUrl)
    yield AppConfig(
      OAuthConfig(clientId, clientSecret, publicBaseUrl),
      FirestoreConfig(projectId, databaseId),
      googleServices(environment),
      apple
    )

  /** GOOGLE_SERVICES is optional: missing or empty requests no additional Google API entitlements during login. */
  private[auth] def googleServices(environment: Map[String, String]): Seq[String] =
    environment.getOrElse("GOOGLE_SERVICES", "").split(",").map(_.trim).filter(_.nonEmpty).toSeq

  private val appleVariables = Seq(
    "APPLE_TEAM_ID",
    "APPLE_KEY_ID",
    "APPLE_PRIVATE_KEY_BASE64",
    "APPLE_IOS_CLIENT_ID",
    "APPLE_SERVICE_ID"
  )

  private def appleOAuth(
      environment: Map[String, String],
      publicBaseUrl: String
  ): Either[IllegalArgumentException, Option[AppleOAuthConfig]] =
    val present =
      appleVariables.flatMap(name => environment.get(name).map(_.trim).filter(_.nonEmpty).map(name -> _)).toMap
    if present.isEmpty then Right(None)
    else
      val missing = appleVariables.filterNot(present.contains)
      if missing.nonEmpty then
        Left(
          new IllegalArgumentException(s"Sign in with Apple is partially configured; missing ${missing.mkString(", ")}")
        )
      else
        try
          val privateKey = new String(Base64.getDecoder.decode(present("APPLE_PRIVATE_KEY_BASE64")), UTF_8).trim
          if !privateKey.contains("BEGIN PRIVATE KEY") then
            Left(new IllegalArgumentException("APPLE_PRIVATE_KEY_BASE64 does not contain a PEM PKCS#8 private key"))
          else
            Right(
              Some(
                AppleOAuthConfig(
                  present("APPLE_TEAM_ID"),
                  present("APPLE_KEY_ID"),
                  privateKey,
                  present("APPLE_IOS_CLIENT_ID"),
                  present("APPLE_SERVICE_ID"),
                  publicBaseUrl
                )
              )
            )
        catch
          case _: IllegalArgumentException =>
            Left(new IllegalArgumentException("APPLE_PRIVATE_KEY_BASE64 is not valid base64"))

  private val localHosts = Set("localhost", "127.0.0.1", "::1")

  private[auth] def validatePublicBaseUrl(value: String): Either[IllegalArgumentException, String] =
    def invalid(reason: String) =
      Left(new IllegalArgumentException(s"Environment variable PUBLIC_BASE_URL $reason"))

    try
      val uri = URI.create(value)
      val scheme = Option(uri.getScheme).map(_.toLowerCase)
      val host = Option(uri.getHost)
        .map(_.toLowerCase)
        .map: value =>
          if value.startsWith("[") && value.endsWith("]") then value.drop(1).dropRight(1) else value
      val path = Option(uri.getRawPath).getOrElse("")
      val port = uri.getPort

      (scheme, host) match
        case (None, _)                                                       => invalid("must use http or https")
        case (Some(protocol), _) if !Set("http", "https").contains(protocol) =>
          invalid("must use http or https")
        case (_, None)                                                => invalid("must contain a valid host")
        case _ if Option(uri.getRawUserInfo).nonEmpty                 => invalid("must not contain user information")
        case _ if path.nonEmpty && path != "/"                        => invalid("must not contain a path")
        case _ if Option(uri.getRawQuery).nonEmpty                    => invalid("must not contain a query")
        case _ if Option(uri.getRawFragment).nonEmpty                 => invalid("must not contain a fragment")
        case _ if port == 0 || port > 65535                           => invalid("contains an invalid port")
        case (Some("http"), Some(name)) if !localHosts.contains(name) =>
          invalid("must use https unless its host is localhost, 127.0.0.1, or ::1")
        case (Some(protocol), Some(name)) =>
          val normalizedHost = if name.contains(':') then s"[$name]" else name
          val portSuffix = Option.when(port >= 0)(s":$port").getOrElse("")
          Right(s"$protocol://$normalizedHost$portSuffix")
    catch case _: IllegalArgumentException => invalid("must be an absolute URL")
