import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import sbt.IO

/** Makes Sign in with Apple an all-or-nothing optional deployment feature.
  *
  * The `.p8` key stays outside the repository and Docker image. Its base64 representation is passed directly to the
  * process (or Cloud Run revision) with the public Apple identifiers that select it.
  */
object AppleBuild {
  private val publicSettings = Seq("APPLE_TEAM_ID", "APPLE_KEY_ID", "APPLE_IOS_CLIENT_ID", "APPLE_SERVICE_ID")

  def configEnv(config: AppConfigBuild.Config, privateKey: Option[File]): Map[String, String] = {
    val configured = publicSettings.flatMap(name => config.optional(name).map(name -> _)).toMap
    val anythingConfigured = configured.nonEmpty || privateKey.nonEmpty
    if (!anythingConfigured) Map.empty
    else {
      val missing = publicSettings.filterNot(configured.contains) ++
        (if (privateKey.isEmpty) Seq("APPLE_PRIVATE_KEY_PATH") else Seq.empty)
      if (missing.nonEmpty)
        sys.error(s"Sign in with Apple is partially configured; missing ${missing.mkString(", ")}")
      val keyBytes = IO.readBytes(privateKey.get)
      val keyText = new String(keyBytes, UTF_8)
      if (!keyText.contains("BEGIN PRIVATE KEY"))
        sys.error(s"Apple private key ${privateKey.get.getAbsolutePath} is not a PEM PKCS#8 private key")
      configured + ("APPLE_PRIVATE_KEY_BASE64" -> Base64.getEncoder.encodeToString(keyBytes))
    }
  }
}
