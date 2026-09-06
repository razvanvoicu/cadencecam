import java.io.File
import sbt.IO

object LocalConfigBuild {
  def requiredPath(configDir: File, prefix: String, baseDir: File): File =
    configuredPath(configDir, prefix, baseDir).getOrElse(
      missing(configDir, prefix)
    )

  def optionalPath(configDir: File, prefix: String, baseDir: File): Option[File] =
    configuredEntry(configDir, prefix).map { case (configFile, path) =>
      val configured = new File(path)
      val resolved =
        (if (configured.isAbsolute) configured else new File(baseDir, path)).getCanonicalFile
      if (!resolved.isFile)
        sys.error(
          s"${configFile.getAbsolutePath} points to a file that does not exist: ${resolved.getAbsolutePath}"
        )
      resolved
    }

  def requiredValue(configDir: File, prefix: String): String =
    configuredEntry(configDir, prefix).map(_._2).getOrElse(missing(configDir, prefix))

  private def configuredPath(configDir: File, prefix: String, baseDir: File): Option[File] =
    optionalPath(configDir, prefix, baseDir)

  private def configuredEntry(configDir: File, prefix: String): Option[(File, String)] = {
    val matches = Option(configDir.listFiles()).toSeq.flatten
      .filter(_.isFile)
      .sortBy(_.getName)
      .flatMap { configFile =>
        IO.readLines(configFile)
          .headOption
          .filter(_.startsWith(prefix))
          .map(line => configFile -> line.drop(prefix.length))
      }

    matches match {
      case Seq()                                      => None
      case Seq((configFile, value)) if value.nonEmpty => Some(configFile -> value)
      case Seq((configFile, _))                       =>
        sys.error(s"${configFile.getAbsolutePath} has an empty $prefix value.")
      case several =>
        sys.error(
          s"Multiple files directly under ${configDir.getAbsolutePath} start with $prefix: " +
            several.map(_._1.getName).mkString(", ")
        )
    }
  }

  private def missing(configDir: File, prefix: String): Nothing =
    sys.error(
      s"No regular file directly under ${configDir.getAbsolutePath} has a first line starting with $prefix"
    )
}
