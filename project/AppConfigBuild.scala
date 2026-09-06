import java.io.File
import scala.util.Try
import sbt.IO

object AppConfigBuild {
  final case class Config private[AppConfigBuild] (source: File, private val values: Map[String, String]) {
    def required(name: String): String =
      values.get(name).map(_.trim).filter(_.nonEmpty).getOrElse {
        sys.error(s"${source.getAbsolutePath} is missing a non-empty $name setting")
      }

    def optional(name: String): Option[String] =
      values.get(name).map(_.trim) match {
        case Some("") => sys.error(s"${source.getAbsolutePath} has an empty $name setting")
        case value    => value
      }

    def requiredPath(name: String): File = resolvedPath(name, required(name))

    def optionalPath(name: String): Option[File] = optional(name).map(resolvedPath(name, _))

    def requiredPort(name: String): String = {
      val value = required(name)
      Try(value.toInt).toOption
        .filter(port => port >= 1 && port <= 65535)
        .fold(
          sys.error(s"${source.getAbsolutePath} has invalid $name '$value'; expected an integer from 1 to 65535")
        )(_ => value)
    }

    private def resolvedPath(name: String, configuredValue: String): File = {
      val configured = new File(configuredValue)
      val resolved =
        (if (configured.isAbsolute) configured else new File(source.getParentFile, configuredValue)).getCanonicalFile
      if (!resolved.isFile)
        sys.error(s"${source.getAbsolutePath} setting $name points to a missing file: ${resolved.getAbsolutePath}")
      resolved
    }
  }

  def load(file: File, supportedKeys: Set[String]): Config = {
    val entries = IO
      .readLines(file)
      .zipWithIndex
      .map { case (line, index) => line.trim -> (index + 1) }
      .filterNot { case (line, _) => line.isEmpty || line.startsWith("#") }
      .map { case (line, lineNumber) =>
        line.split("=", 2) match {
          case Array(name, value) if name.trim.nonEmpty => (name.trim, value.trim, lineNumber)
          case _ => sys.error(s"Malformed configuration at ${file.getAbsolutePath}:$lineNumber; expected NAME=value")
        }
      }
    val duplicates = entries
      .groupBy(_._1)
      .collect {
        case (name, occurrences) if occurrences.size > 1 => name
      }
      .toSeq
    if (duplicates.nonEmpty)
      sys.error(s"Duplicate settings in ${file.getAbsolutePath}: ${duplicates.sorted.mkString(", ")}")
    val unknown = entries.map(_._1).toSet -- supportedKeys
    if (unknown.nonEmpty)
      sys.error(s"Unknown settings in ${file.getAbsolutePath}: ${unknown.toSeq.sorted.mkString(", ")}")
    Config(file.getCanonicalFile, entries.map { case (name, value, _) => name -> value }.toMap)
  }

  def gcpRuntimeEnv(config: Config): Map[String, String] =
    Seq("GCP_PROJECT_ID", "FIRESTORE_DATABASE_ID", "FIRESTORE_LOCATION")
      .map(name => name -> config.required(name))
      .toMap
}
