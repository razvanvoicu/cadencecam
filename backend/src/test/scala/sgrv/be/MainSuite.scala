package sgrv.be

import com.google.gson.JsonParser
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import zio.{Runtime, Task, Unsafe, ZIO}
import zio.http.{Header, MediaType, Request, Status, URL}

class MainSuite extends munit.FunSuite:

  private val testStaticCacheControl = Header.CacheControl.MaxAge(300)

  private def run[A](effect: Task[A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(effect).getOrThrowFiberFailure()
    }

  private def resourceText(name: String): String =
    String(resourceBytes(name), UTF_8)

  private def resourceBytes(name: String): Array[Byte] =
    val input = Option(getClass.getClassLoader.getResourceAsStream(name)).getOrElse(fail(s"Missing resource: $name"))
    try input.readAllBytes()
    finally input.close()

  private def pngSize(bytes: Array[Byte]): (Int, Int) =
    assert(bytes.length >= 24, "PNG is too short to contain an IHDR chunk")
    assertEquals(bytes.slice(1, 4).map(_.toChar).mkString, "PNG")
    val dimensions = ByteBuffer.wrap(bytes, 16, 8)
    dimensions.getInt() -> dimensions.getInt()

  test("binds the HTTP server to a given host and port"):
    val config = Main.serverConfig("0.0.0.0", 9000)
    val address = config.address

    assertEquals(address.getAddress.getHostAddress, "0.0.0.0")
    assertEquals(address.getPort, 9000)
    assertEquals(config.gracefulShutdownTimeout, Config.serverShutdownTimeout)
    assertEquals(Main.gracefulShutdownTimeout, Config.processShutdownTimeout)

  test("delivers the container SIGTERM directly to Java as PID 1"):
    val dockerfile = resourceText("Dockerfile")
    val launcher = resourceText("runApp")

    assert(dockerfile.linesIterator.exists(_.trim == "STOPSIGNAL SIGTERM"))
    assert(dockerfile.linesIterator.exists(_.trim == "ENTRYPOINT [\"/app/runApp\"]"))
    assert(launcher.linesIterator.exists(_.trim.startsWith("exec java ")))

  test("keeps tracked environment baselines free of task-specific origins and injected runtime secrets"):
    val prodEnv = resourceText("prod.env").linesIterator.map(_.trim).filter(_.nonEmpty).toSeq
    val testEnv = resourceText("test.env").linesIterator.map(_.trim).filter(_.nonEmpty).toSeq
    val externallyConfiguredKeys = Seq(
      "PORT=",
      "LOCAL_BASE_URL=",
      "ARTIFACT_BASE_URL=",
      "PUBLIC_BASE_URL=",
      "ARTIFACT_PORT=",
      "GCP_PROJECT_ID=",
      "FIRESTORE_DATABASE_ID=",
      "FIRESTORE_LOCATION=",
      "GCLOUD_REGION=",
      "ARTIFACT_REGISTRY_REPOSITORY=",
      "GCLOUD_SERVICE_ACCOUNT=",
      "GOOGLE_OAUTH_CLIENT_ID=",
      "GOOGLE_OAUTH_CLIENT_SECRET=",
      "ADMIN_PASSWORD="
    )

    externallyConfiguredKeys.foreach(prefix =>
      assert(!prodEnv.exists(_.startsWith(prefix)), s"Found $prefix in prod.env")
      assert(!testEnv.exists(_.startsWith(prefix)), s"Found $prefix in test.env")
    )

  test("requires PORT and accepts a valid environment value"):
    assertEquals(Main.port(Some("7000")), Right(7000))
    assertEquals(Main.port(Some(" 7000 ")), Right(7000))
    assertEquals(
      Main.port(None).swap.toOption.get.getMessage,
      "Environment variable PORT is not set or is empty; startup cannot continue."
    )
    assertEquals(
      Main.port(Some("  ")).swap.toOption.get.getMessage,
      "Environment variable PORT is not set or is empty; startup cannot continue."
    )

  test("rejects an invalid PORT value"):
    val nonNumericError = Main.port(Some("invalid")).swap.toOption.get
    val outOfRangeError = Main.port(Some("70000")).swap.toOption.get

    assertEquals(
      nonNumericError.getMessage,
      "Invalid port configuration from PORT: 'invalid'. Expected an integer from 1 to 65535."
    )
    assertEquals(
      outOfRangeError.getMessage,
      "Invalid port configuration from PORT: '70000'. Expected an integer from 1 to 65535."
    )

  test("accepts only ports in the TCP range"):
    assert(Main.port(Some("1")).isRight)
    assert(Main.port(Some("65535")).isRight)
    assert(Main.port(Some("0")).isLeft)
    assert(Main.port(Some("-1")).isLeft)
    assert(Main.port(Some("65536")).isLeft)

  test("requires a non-negative static asset cache duration"):
    assertEquals(Main.staticCacheControl(Some("300")), Right(Header.CacheControl.MaxAge(300)))
    assertEquals(Main.staticCacheControl(Some(" 86400 ")), Right(Header.CacheControl.MaxAge(86400)))
    assertEquals(
      Main.staticCacheControl(None).swap.toOption.get.getMessage,
      "Environment variable STATIC_ASSET_CACHE_MAX_AGE_SECONDS is not set or is empty; startup cannot continue."
    )
    assertEquals(
      Main.staticCacheControl(Some("invalid")).swap.toOption.get.getMessage,
      "Invalid STATIC_ASSET_CACHE_MAX_AGE_SECONDS value 'invalid'. Expected a non-negative integer."
    )
    assert(Main.staticCacheControl(Some("-1")).isLeft)

  test("uses a five-minute local cache and a one-day production cache"):
    val prodEnv = resourceText("prod.env").linesIterator.map(_.trim).toSet
    val testEnv = resourceText("test.env").linesIterator.map(_.trim).toSet

    assert(prodEnv.contains("STATIC_ASSET_CACHE_MAX_AGE_SECONDS=86400"))
    assert(testEnv.contains("STATIC_ASSET_CACHE_MAX_AGE_SECONDS=300"))

  test("selects the bind address from the environment, defaulting to IPv4 loopback"):
    assertEquals(Main.bindAddress(Some("0.0.0.0")), "0.0.0.0")
    assertEquals(Main.bindAddress(Some("  ")), "127.0.0.1")
    assertEquals(Main.bindAddress(None), "127.0.0.1")

  test("formats request annotations as a compact route summary"):
    val annotations = Map(
      "method" -> "GET",
      "duration_ms" -> "70",
      "url" -> "/debug",
      "response_size" -> "12392",
      "status_code" -> "200",
      "request_size" -> "0"
    )

    assertEquals(LoggerConfig.requestSummary(annotations), Some(" [GET /debug -> 200 70ms]"))
    assertEquals(LoggerConfig.requestSummary(Map.empty), None)

  test("loads packaged assets and returns not found for a missing asset"):
    val index = run(Main.asset("index.html", MediaType.text.html, testStaticCacheControl))
    val content = run(index.body.asString)
    val missing = run(Main.asset("missing.txt", MediaType.text.plain, testStaticCacheControl))

    assertEquals(index.status, Status.Ok)
    assert(content.contains("<html"))
    assert(content.contains("href=\"/favicon.ico\""))
    assert(content.contains("rel=\"manifest\" href=\"/manifest.webmanifest\""))
    assertEquals(index.headers.get(Header.CacheControl), Some(testStaticCacheControl))
    assertEquals(missing.status, Status.NotFound)

  test("serves a static asset through the application routes"):
    val routes = run(ZIO.succeed(Main.staticRoutes(testStaticCacheControl)))
    val request = Request.get(URL.decode("/style.css").toOption.get)
    val response = run(zio.ZIO.scoped(routes.runZIO(request)))
    val content = run(response.body.asString)
    val index = run(zio.ZIO.scoped(routes.runZIO(Request.get(URL.decode("/").toOption.get))))
    val productionRoutes = run(ZIO.succeed(Main.staticRoutes(Header.CacheControl.MaxAge(86400))))
    val productionResponse = run(zio.ZIO.scoped(productionRoutes.runZIO(request)))

    assertEquals(response.status, Status.Ok)
    assertEquals(response.headers.get(Header.CacheControl), Some(testStaticCacheControl))
    assert(content.nonEmpty)
    assertEquals(index.headers.get(Header.CacheControl), Some(Header.CacheControl.NoCache))
    assertEquals(productionResponse.headers.get(Header.CacheControl), Some(Header.CacheControl.MaxAge(86400)))

  test("packages and serves the favicon as a cached Microsoft icon"):
    val packaged = resourceBytes("web/favicon.ico")
    val routes = run(ZIO.succeed(Main.staticRoutes(testStaticCacheControl)))
    val response = run(ZIO.scoped(routes.runZIO(Request.get(URL.decode("/favicon.ico").toOption.get))))
    val served = run(response.body.asArray)

    assert(packaged.length > 4)
    assertEquals(packaged.take(4).toSeq, Seq[Byte](0, 0, 1, 0))
    assertEquals(response.status, Status.Ok)
    assertEquals(
      response.headers.get(Header.ContentType),
      Some(Header.ContentType(MediaType.image.`vnd.microsoft.icon`))
    )
    assertEquals(response.headers.get(Header.CacheControl), Some(testStaticCacheControl))
    assertEquals(served.toSeq, packaged.toSeq)

  test("packages and serves an installable PWA manifest with the configured cache duration"):
    val routes = run(ZIO.succeed(Main.staticRoutes(testStaticCacheControl)))
    val response = run(ZIO.scoped(routes.runZIO(Request.get(URL.decode("/manifest.webmanifest").toOption.get))))
    val manifest = run(response.body.asString)
    val json = JsonParser.parseString(manifest).getAsJsonObject
    val icons = json.getAsJsonArray("icons")
    val iconSizes = (0 until icons.size()).map(index => icons.get(index).getAsJsonObject.get("sizes").getAsString).toSet

    assertEquals(response.status, Status.Ok)
    assertEquals(
      response.headers.get(Header.ContentType),
      Some(Header.ContentType(MediaType.application.`manifest+json`))
    )
    assertEquals(response.headers.get(Header.CacheControl), Some(testStaticCacheControl))
    assertEquals(json.get("name").getAsString, "Web App Template")
    assertEquals(json.get("start_url").getAsString, "/")
    assertEquals(json.get("scope").getAsString, "/")
    assertEquals(json.get("display").getAsString, "standalone")
    assertEquals(iconSizes, Set("192x192", "512x512"))

  test("packages and serves the required PWA icons as PNG files"):
    val routes = run(ZIO.succeed(Main.staticRoutes(testStaticCacheControl)))

    Seq("icon-192.png" -> (192, 192), "icon-512.png" -> (512, 512)).foreach { case (fileName, expectedSize) =>
      val packaged = resourceBytes(s"web/$fileName")
      val response = run(ZIO.scoped(routes.runZIO(Request.get(URL.decode(s"/$fileName").toOption.get))))
      val served = run(response.body.asArray)

      assertEquals(pngSize(packaged), expectedSize)
      assertEquals(response.status, Status.Ok)
      assertEquals(response.headers.get(Header.ContentType), Some(Header.ContentType(MediaType.image.png)))
      assertEquals(response.headers.get(Header.CacheControl), Some(testStaticCacheControl))
      assertEquals(served.toSeq, packaged.toSeq)
    }
