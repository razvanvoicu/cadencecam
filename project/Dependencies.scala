//noinspection scala2InSource3
import sbt._

object Dependencies {
  val munitVersion = "1.3.4"
  lazy val classGraph = "io.github.classgraph" % "classgraph" % "4.8.181"
  lazy val zioHttp = "dev.zio" %% "zio-http" % "3.11.3"
  lazy val zioLogging = "dev.zio" %% "zio-logging" % "2.5.3"
  lazy val munit = "org.scalameta" %% "munit" % munitVersion
  val zioJsonVersion = "0.9.1"

  // Google Cloud: Firestore access-log persistence and OAuth 2.0 code exchange / ID-token verification
  lazy val firestore = "com.google.cloud" % "google-cloud-firestore" % "3.43.0"
  lazy val firestoreAdmin = "com.google.cloud" % "google-cloud-firestore-admin" % "3.43.0"
  lazy val googleApiClient = "com.google.api-client" % "google-api-client" % "2.8.0"
  // JSON for the Sheets/Drive REST calls; already on the classpath transitively via google-api-client's
  // GsonFactory, declared explicitly so the dependency doesn't rely on that transitive detail.
  lazy val gson = "com.google.code.gson" % "gson" % "2.10.1"

  // Browser automation for the e2etest project; Selenium Manager (bundled since 4.6) auto-resolves a matching
  // chromedriver for whatever Chrome is installed, so no separate driver-binary install/PATH management.
  lazy val selenium = "org.seleniumhq.selenium" % "selenium-java" % "4.27.0"

  // Scala.js artifacts are declared with `%%%` in build.sbt, which needs a
  // settings scope, so only their versions live here.
  val laminarVersion = "17.2.1"
  val scalajsDomVersion = "2.8.1"
}
