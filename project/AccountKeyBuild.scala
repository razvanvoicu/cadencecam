import java.io.File
import sbt.IO

/** Reads the secret that names account documents, and hands it to the running service as an environment variable.
  *
  * Outside the repository and outside the Docker image, like the OAuth credentials: the file is read while a revision
  * is being created and its contents travel no further than that revision's environment.
  */
object AccountKeyBuild {
  def configEnv(file: File): Map[String, String] = {
    if (!file.isFile)
      sys.error(
        s"Account key file does not exist: ${file.getAbsolutePath}. " +
          "Set ACCOUNT_KEY_PATH in the shared application configuration."
      )

    val key = IO.read(file).trim
    if (key.isEmpty)
      sys.error(s"Account key file ${file.getAbsolutePath} is empty.")
    if (key.exists(character => character == '\r' || character == '\n'))
      sys.error(s"Account key file ${file.getAbsolutePath} must be a single line with no line breaks.")
    // Short keys are the failure that looks like it works: everything runs, and the names are simply easier to attack.
    if (key.length < 32)
      sys.error(
        s"Account key file ${file.getAbsolutePath} holds ${key.length} characters; at least 32 are required. " +
          "Generate one with: openssl rand -hex 32"
      )

    Map("ACCOUNT_KEY" -> key)
  }
}
