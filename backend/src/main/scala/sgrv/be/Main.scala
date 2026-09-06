package sgrv.be

import sgrv.be.auth.{AppConfig, DatabaseAdmin, GoogleOAuth, SessionStore, TokenGenerator}
import sgrv.be.core.{CapabilityRegistry, RouteDiscovery}
import zio.*
import zio.http.*
import zio.logging.*

private[be] object Config:
  val serverShutdownTimeout: Duration = 8.seconds
  val processShutdownTimeout: Duration = 9.seconds
  final case class MissingPort()
      extends IllegalArgumentException("Environment variable PORT is not set or is empty; startup cannot continue.")
  final case class InvalidPort(source: String, value: String)
      extends IllegalArgumentException(
        s"Invalid port configuration from $source: '$value'. Expected an integer from 1 to 65535."
      )
  final case class MissingStaticAssetCacheMaxAge()
      extends IllegalArgumentException(
        "Environment variable STATIC_ASSET_CACHE_MAX_AGE_SECONDS is not set or is empty; startup cannot continue."
      )
  final case class InvalidStaticAssetCacheMaxAge(value: String)
      extends IllegalArgumentException(
        s"Invalid STATIC_ASSET_CACHE_MAX_AGE_SECONDS value '$value'. Expected a non-negative integer."
      )
  val defaultBindAddress = "127.0.0.1"

private[be] object LoggerConfig:
  private val request = LogFormat.make: (builder, _, _, _, _, _, _, _, annotations) =>
    requestSummary(annotations).foreach(builder.appendText)
  private val logFormat = LogFormat.timestamp |-| LogFormat.level |-| LogFormat.line +
    (LogFormat.space + LogFormat.cause).filter(LogFilter.causeNonEmpty) + request
  private[be] val loggerConfig = ConsoleLoggerConfig(logFormat, LogFilter.LogLevelByNameConfig(LogLevel.Trace))

  private[be] def requestSummary(annotations: Map[String, String]): Option[String] =
    for
      method <- annotations.get("method")
      url <- annotations.get("url")
      status <- annotations.get("status_code")
      duration <- annotations.get("duration_ms")
    yield s" [$method $url -> $status ${duration}ms]"

object Main extends ZIOAppDefault:
  import Config.*
  import LoggerConfig.*
  import Body.fromArray
  import zio.http.Header.{CacheControl, ContentType}
  import MediaType.{application, image, text}
  import Status.{NotFound, InternalServerError}

  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] = Runtime.removeDefaultLoggers >>> consoleLogger(loggerConfig)
  override val gracefulShutdownTimeout: Duration = processShutdownTimeout

  private[be] def asset(fName: String, mediaType: MediaType, cacheCtrl: CacheControl): UIO[Response] =
    val makeResponse: Option[Array[Byte]] => Response =
      case Some(bytes) => Response(headers = Headers(ContentType(mediaType), cacheCtrl), body = fromArray(bytes))
      case None        => Response.status(NotFound)
    val bytes = ZIO
      .attemptBlockingIO(Option(getClass.getClassLoader.getResourceAsStream(s"web/$fName")))
      .flatMap:
        case None        => ZIO.none
        case Some(input) =>
          ZIO.acquireReleaseWith(ZIO.succeed(input))(stream => ZIO.attemptBlockingIO(stream.close()).ignore)(stream =>
            ZIO.attemptBlockingIO(stream.readAllBytes()).asSome
          )
    bytes.fold(_ => Response.status(InternalServerError), makeResponse)

  private val index = asset("index.html", text.html, CacheControl.NoCache)

  private[be] def staticRoutes(staticCacheCtrl: CacheControl) = Routes(
    Method.GET / "" -> handler(index),
    Method.GET / "index.html" -> handler(index),
    Method.GET / "favicon.ico" -> handler(asset("favicon.ico", image.`vnd.microsoft.icon`, staticCacheCtrl)),
    Method.GET / "icon-192.png" -> handler(asset("icon-192.png", image.png, staticCacheCtrl)),
    Method.GET / "icon-512.png" -> handler(asset("icon-512.png", image.png, staticCacheCtrl)),
    Method.GET / "manifest.webmanifest" ->
      handler(asset("manifest.webmanifest", application.`manifest+json`, staticCacheCtrl)),
    Method.GET / "style.css" -> handler(asset("style.css", text.css, staticCacheCtrl)),
    Method.GET / "main.js" -> handler(asset("main.js", text.javascript, staticCacheCtrl)),
    Method.GET / "main.js.map" -> handler(asset("main.js.map", application.json, staticCacheCtrl))
  )

  private def port: IO[IllegalArgumentException, Int] =
    System.env("PORT").orElseSucceed(None).flatMap(environmentPort => ZIO.fromEither(port(environmentPort)))

  private[be] def port(environmentPort: Option[String]): Either[IllegalArgumentException, Int] =
    environmentPort.map(_.trim).filter(_.nonEmpty) match
      case None        => Left(MissingPort())
      case Some(value) =>
        value.toIntOption.filter(port => port >= 1 && port <= 65535).toRight(InvalidPort("PORT", value))

  private def staticCacheControl: IO[IllegalArgumentException, CacheControl] =
    System
      .env("STATIC_ASSET_CACHE_MAX_AGE_SECONDS")
      .orElseSucceed(None)
      .flatMap(environmentValue => ZIO.fromEither(staticCacheControl(environmentValue)))

  private[be] def staticCacheControl(environmentValue: Option[String]): Either[IllegalArgumentException, CacheControl] =
    environmentValue.map(_.trim).filter(_.nonEmpty) match
      case None        => Left(MissingStaticAssetCacheMaxAge())
      case Some(value) =>
        value.toIntOption.filter(_ >= 0).map(CacheControl.MaxAge.apply).toRight(InvalidStaticAssetCacheMaxAge(value))

  private def bindAddress: UIO[String] =
    System.env("BIND_ADDRESS").orElseSucceed(None).map(bindAddress)

  private[be] def bindAddress(environmentValue: Option[String]): String =
    environmentValue.map(_.trim).filter(_.nonEmpty).getOrElse(defaultBindAddress)

  private[be] def serverConfig(host: String, port: Int): Server.Config =
    Server.Config.default.binding(host, port).gracefulShutdownTimeout(serverShutdownTimeout)

  private type Layer = BackendEnvironment & DatabaseAdmin
  private val backendLayer: ZLayer[Any, Throwable, Layer] =
    ZLayer.make[Layer](
      AppConfig.live,
      GoogleOAuth.live,
      SessionStore.live,
      TokenGenerator.live,
      DatabaseAdmin.live,
      Client.default
    )

  // noinspection HttpUrlsUsage
  def run: ZIO[ZIOAppArgs, Any, Any] =
    val unit = for
      p <- port
      host <- bindAddress
      staticCacheCtrl <- staticCacheControl
      environment <- ZIO.environment[BackendEnvironment]
      registry = CapabilityRegistry.fromEnvironment(environment)
      static = staticRoutes(staticCacheCtrl)
      reservedPatterns = static.routes.map(_.routePattern: Any).toSet
      applicationRoutes <- RouteDiscovery.routes(registry, reservedPatterns).map(static ++ _)
      _ <- DatabaseAdmin.ensureDatabase.catchAll: error =>
        ZIO.logWarning(s"Could not ensure the Firestore database: ${error.getMessage}")
      _ <- DatabaseAdmin.ensureSessionTtl.catchAll: error =>
        ZIO.logWarning(s"Could not ensure the Firestore session TTL policy: ${error.getMessage}")
      _ <- ZIO.logInfo(s"Serving on http://$host:$p/")
      _ <- Server
        .serve(applicationRoutes @@ HandlerAspect.requestLogging())
        .onInterrupt(
          ZIO.logInfo(
            s"Shutdown requested; active HTTP requests have up to ${serverShutdownTimeout.render} to complete"
          )
        )
        .provide(Server.defaultWith(_ => serverConfig(host, p)))
    yield ()
    unit.provide(backendLayer)
