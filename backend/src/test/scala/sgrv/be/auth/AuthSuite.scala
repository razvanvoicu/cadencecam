package sgrv.be.auth

import com.google.firestore.admin.v1.Field
import java.time.Instant
import scala.jdk.CollectionConverters.*
import zio.http.{Path, Method as ZioMethod}

class AuthSuite extends munit.FunSuite:

  test("validates and normalizes the configured public base URL"):
    assertEquals(AppConfig.validatePublicBaseUrl("http://localhost:8123/"), Right("http://localhost:8123"))
    assertEquals(AppConfig.validatePublicBaseUrl("http://127.0.0.1:8123"), Right("http://127.0.0.1:8123"))
    assertEquals(AppConfig.validatePublicBaseUrl("http://[::1]:8123"), Right("http://[::1]:8123"))
    assertEquals(AppConfig.validatePublicBaseUrl("https://APP.EXAMPLE.COM:8443"), Right("https://app.example.com:8443"))

  test("rejects unsafe or ambiguous public base URLs"):
    Seq(
      "http://app.example.com",
      "https://user@app.example.com",
      "https://app.example.com/base",
      "https://app.example.com?query=yes",
      "https://app.example.com#fragment",
      "https://app.example.com:0",
      "app.example.com"
    ).foreach(value => assert(AppConfig.validatePublicBaseUrl(value).isLeft, value))

  test("builds the Google authorization URL with encoded parameters"):
    val url = GoogleOAuth.authorizationUrl("client-1", "http://localhost:8123/auth/callback", "state/value")

    assert(url.startsWith("https://accounts.google.com/o/oauth2/v2/auth?"))
    assert(url.contains("client_id=client-1"))
    assert(url.contains("redirect_uri=http%3A%2F%2Flocalhost%3A8123%2Fauth%2Fcallback"))
    assert(url.contains("response_type=code"))
    assert(url.contains("scope=openid+email+profile"))
    assert(url.contains("state=state%2Fvalue"))
    assert(url.contains("access_type=offline"))
    assert(url.contains("prompt=select_account+consent"))

  test("appends configured Google service scopes to the authorization URL"):
    val url = GoogleOAuth.authorizationUrl(
      "client-1",
      "http://localhost:8123/auth/callback",
      "state",
      extraScopes = Seq("https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive.file")
    )

    assert(
      url.contains(
        "scope=openid+email+profile+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fspreadsheets" +
          "+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fdrive.file"
      )
    )

  test("loads and trims the environment-supplied OAuth configuration"):
    val configuration = AppConfig.fromEnvironment(
      Map(
        "GOOGLE_OAUTH_CLIENT_ID" -> " client-id ",
        "GOOGLE_OAUTH_CLIENT_SECRET" -> " client-secret ",
        "PUBLIC_BASE_URL" -> " https://app.example.com/ ",
        "GCP_PROJECT_ID" -> " project-id ",
        "FIRESTORE_DATABASE_ID" -> " database-id ",
        "FIRESTORE_LOCATION" -> " location "
      )
    )

    assertEquals(
      configuration,
      Right(
        AppConfig(
          OAuthConfig("client-id", "client-secret", "https://app.example.com"),
          FirestoreConfig("project-id", "database-id", "location")
        )
      )
    )

  test("requires the public base URL"):
    val error = AppConfig
      .fromEnvironment(
        Map(
          "GOOGLE_OAUTH_CLIENT_ID" -> "client-id",
          "GOOGLE_OAUTH_CLIENT_SECRET" -> "client-secret"
        )
      )
      .left
      .toOption
      .get

    assertEquals(error.getMessage, "Environment variable PUBLIC_BASE_URL is not set or is empty; see prod.env")

  test("parses GOOGLE_SERVICES as a trimmed, comma-separated scope list"):
    assertEquals(AppConfig.googleServices(Map.empty), Seq.empty)
    assertEquals(AppConfig.googleServices(Map("GOOGLE_SERVICES" -> "")), Seq.empty)
    assertEquals(
      AppConfig.googleServices(Map("GOOGLE_SERVICES" -> " scope-a , scope-b ,,scope-c")),
      Seq("scope-a", "scope-b", "scope-c")
    )

  test("rejects an incomplete OAuth configuration"):
    val error = AppConfig.fromEnvironment(Map("GOOGLE_OAUTH_CLIENT_ID" -> "client-id")).left.toOption.get
    assertEquals(
      error.getMessage,
      "Environment variable GOOGLE_OAUTH_CLIENT_SECRET is not set or is empty; see prod.env"
    )

  test("falls back to the mailbox name when Google supplies no display name"):
    assertEquals(GoogleOAuth.displayName(Some("Jane Doe"), "jane@example.com"), "Jane Doe")
    assertEquals(GoogleOAuth.displayName(Some("   "), "jane@example.com"), "jane")
    assertEquals(GoogleOAuth.displayName(None, "jane.doe@example.com"), "jane.doe")
    assertEquals(GoogleOAuth.displayName(None, "@example.com"), "@example.com")

  test("serialises the signed-in user as escaped JSON"):
    assertEquals(Me.json(SessionUser("a@b.c", "Jane \"JJ\" Doe")), """{"email":"a@b.c","name":"Jane \"JJ\" Doe"}""")

  test("configures expiresAt as the Access collection TTL field"):
    val config = FirestoreConfig("project-id", "database-id", "location")
    val request = DatabaseAdmin.sessionTtlUpdateRequest(config)

    assertEquals(
      request.getField.getName,
      "projects/project-id/databases/database-id/collectionGroups/Access/fields/expiresAt"
    )
    assert(request.getField.hasTtlConfig)
    assertEquals(request.getUpdateMask.getPathsList.asScala.toSeq, Seq("ttl_config"))

  test("repairs only missing or failed session TTL configurations"):
    val active = Field
      .newBuilder()
      .setTtlConfig(Field.TtlConfig.newBuilder().setState(Field.TtlConfig.State.ACTIVE))
      .build()
    val creating = Field
      .newBuilder()
      .setTtlConfig(Field.TtlConfig.newBuilder().setState(Field.TtlConfig.State.CREATING))
      .build()
    val needsRepair = Field
      .newBuilder()
      .setTtlConfig(Field.TtlConfig.newBuilder().setState(Field.TtlConfig.State.NEEDS_REPAIR))
      .build()

    assert(DatabaseAdmin.sessionTtlNeedsUpdate(Field.getDefaultInstance))
    assert(!DatabaseAdmin.sessionTtlNeedsUpdate(active))
    assert(!DatabaseAdmin.sessionTtlNeedsUpdate(creating))
    assert(DatabaseAdmin.sessionTtlNeedsUpdate(needsRepair))

  test("persists the OAuth tokens carried by SessionUser without duplicating them"):
    val createdAt = Instant.parse("2026-08-16T00:00:00Z")
    val expiresAt = Instant.parse("2026-08-23T00:00:00Z")
    val withToken = SessionStore.documentFields(
      "session-key",
      SessionUser("jane@example.com", "Jane", Some("refresh-token")),
      createdAt,
      expiresAt
    )
    val withoutToken = SessionStore.documentFields(
      "session-key",
      SessionUser("jane@example.com", "Jane"),
      createdAt,
      expiresAt
    )
    val withRevocationFallback = SessionStore.documentFields(
      "session-key",
      SessionUser("jane@example.com", "Jane", accessTokenForRevocation = Some("access-token")),
      createdAt,
      expiresAt
    )

    assertEquals(withToken.get("refreshToken"), "refresh-token")
    assert(!withToken.containsKey("accessTokenForRevocation"))
    assert(!withoutToken.containsKey("refreshToken"))
    assert(!withoutToken.containsKey("accessTokenForRevocation"))
    assertEquals(withRevocationFallback.get("accessTokenForRevocation"), "access-token")
    assert(!withRevocationFallback.containsKey("refreshToken"))

  test("auth plugins expose their native typed routes"):
    Seq(
      Login.routes.routes.exists(_.routePattern.matches(ZioMethod.GET, Path("/auth/login"))),
      Callback.routes.routes.exists(_.routePattern.matches(ZioMethod.GET, Path("/auth/callback"))),
      Me.routes.routes.exists(_.routePattern.matches(ZioMethod.GET, Path("/me"))),
      RefreshSession.routes.routes.exists(_.routePattern.matches(ZioMethod.POST, Path("/refreshSession"))),
      Logout.routes.routes.exists(_.routePattern.matches(ZioMethod.POST, Path("/logout")))
    ).foreach(assert(_))
