import java.io.File
import scala.util.matching.Regex
import sbt.IO

object OAuthBuild {
  private val jsonStringEscape: Regex = """\\(?:["\\/bfnrt]|u[0-9a-fA-F]{4})""".r

  private def decodeJsonString(value: String): String =
    jsonStringEscape.replaceAllIn(
      value,
      matched =>
        matched.matched match {
          case "\\\""                               => "\""
          case "\\\\"                               => "\\"
          case "\\/"                                => "/"
          case "\\b"                                => "\b"
          case "\\f"                                => "\f"
          case "\\n"                                => "\n"
          case "\\r"                                => "\r"
          case "\\t"                                => "\t"
          case unicode if unicode.startsWith("\\u") => Integer.parseInt(unicode.drop(2), 16).toChar.toString
        }
    )

  def configEnv(file: File): Map[String, String] = {
    if (!file.isFile)
      sys.error(
        s"OAuth configuration file does not exist: ${file.getAbsolutePath}. " +
          "Set OAUTH_CONFIG_PATH in the shared application configuration."
      )

    val json = IO.read(file)
    def requiredJsonString(field: String): String = {
      val pattern = ("(?s)\\\"" + Regex.quote(field) + "\\\"\\s*:\\s*\\\"((?:\\\\.|[^\\\"\\\\])*)\\\"").r
      pattern
        .findFirstMatchIn(json)
        .map(m => decodeJsonString(m.group(1)))
        .map(_.trim)
        .filter(_.nonEmpty)
        .getOrElse(sys.error(s"OAuth configuration file ${file.getAbsolutePath} lacks web.$field"))
    }

    Map(
      "GOOGLE_OAUTH_CLIENT_ID" -> requiredJsonString("client_id"),
      "GOOGLE_OAUTH_CLIENT_SECRET" -> requiredJsonString("client_secret")
    )
  }

  def envLines(values: Map[String, String]): Seq[String] =
    values.toSeq.sortBy(_._1).map { case (key, value) =>
      if (value.exists(character => character == '\r' || character == '\n'))
        sys.error(s"Environment value $key contains a line break and cannot be written to prod.env")
      s"$key=$value"
    }
}
