package sgrv.be.about

import java.util.Properties
import sgrv.api.AboutInfo

private[about] object BuildInformation:
  val current: AboutInfo =
    val resourceName = "sgrv/be/about/build-info.properties"
    val input = Option(getClass.getClassLoader.getResourceAsStream(resourceName)).getOrElse:
      throw new IllegalStateException(s"Missing generated resource $resourceName")
    val properties = new Properties()
    try properties.load(input)
    finally input.close()

    def required(name: String): String =
      Option(properties.getProperty(name))
        .map(_.trim)
        .filter(_.nonEmpty)
        .getOrElse:
          throw new IllegalStateException(s"Generated resource $resourceName is missing $name")

    AboutInfo(
      appVersion = required("app.version"),
      buildDate = required("build.date"),
      buildOs = required("build.os"),
      scalaVersion = required("scala.version"),
      scalaJsVersion = required("scala.js.version")
    )
