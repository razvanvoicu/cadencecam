import java.net.URI

object PublicBaseUrlBuild {
  private val localHosts = Set("localhost", "127.0.0.1", "::1")

  def configEnv(configuredVariable: String, configuredValue: String): Map[String, String] =
    Map("PUBLIC_BASE_URL" -> validate(configuredVariable.stripSuffix("="), configuredValue.trim))

  def localConfigEnv(configuredVariable: String, configuredValue: String): Map[String, String] = {
    val variable = configuredVariable.stripSuffix("=")
    val baseUrl = validate(variable, configuredValue.trim)
    val port = URI.create(baseUrl).getPort
    if (port < 0) sys.error(s"Configured $variable must contain an explicit port")
    Map("PUBLIC_BASE_URL" -> baseUrl, "PORT" -> port.toString)
  }

  private def validate(configuredVariable: String, value: String): String = {
    def invalid(reason: String): Nothing =
      sys.error(s"Configured $configuredVariable $reason")

    if (value.isEmpty) invalid("is empty")

    try {
      val uri = URI.create(value)
      val scheme = Option(uri.getScheme).map(_.toLowerCase)
      val host = Option(uri.getHost).map(_.toLowerCase).map { value =>
        if (value.startsWith("[") && value.endsWith("]")) value.drop(1).dropRight(1) else value
      }
      val path = Option(uri.getRawPath).getOrElse("")
      val port = uri.getPort

      (scheme, host) match {
        case (None, _)                                                       => invalid("must use http or https")
        case (Some(protocol), _) if !Set("http", "https").contains(protocol) =>
          invalid("must use http or https")
        case (_, None)                                                => invalid("must contain a valid host")
        case _ if Option(uri.getRawUserInfo).nonEmpty                 => invalid("must not contain user information")
        case _ if path.nonEmpty && path != "/"                        => invalid("must not contain a path")
        case _ if Option(uri.getRawQuery).nonEmpty                    => invalid("must not contain a query")
        case _ if Option(uri.getRawFragment).nonEmpty                 => invalid("must not contain a fragment")
        case _ if port == 0 || port > 65535                           => invalid("contains an invalid port")
        case (Some("http"), Some(name)) if !localHosts.contains(name) =>
          invalid("must use https unless its host is localhost, 127.0.0.1, or ::1")
        case (Some(protocol), Some(name)) =>
          val normalizedHost = if (name.contains(':')) s"[$name]" else name
          val portSuffix = if (port >= 0) s":$port" else ""
          s"$protocol://$normalizedHost$portSuffix"
      }
    } catch {
      case _: IllegalArgumentException => invalid("must be an absolute URL")
    }
  }
}
