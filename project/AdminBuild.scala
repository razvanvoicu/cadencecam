import java.io.File
import sbt.IO

object AdminBuild {
  def configEnv(file: File): Map[String, String] = {
    if (!file.isFile)
      sys.error(
        s"Admin password file does not exist: ${file.getAbsolutePath}. " +
          "Set ADMIN_PASSWORD_PATH in the shared application configuration."
      )

    val password = IO.read(file).trim
    if (password.isEmpty)
      sys.error(s"Admin password file ${file.getAbsolutePath} is empty.")
    if (password.exists(character => character == '\r' || character == '\n'))
      sys.error(s"Admin password file ${file.getAbsolutePath} must be a single line with no line breaks.")

    Map("ADMIN_PASSWORD" -> password)
  }
}
