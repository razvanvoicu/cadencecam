package sgrv.be.debug

import java.io.File
import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
import zio.{IO, Task, UIO, ZIO}
import zio.http.{Header, Method, Response, Routes, handler}

import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}

/** Sensitive diagnostics supplied as an independently packaged backend plugin. This allows the inspection of a
  * prospective production environment in the cloud, revealing how the cloud provider has set up that environment (e.g.
  * AppEngine, Lambda). This plugin can be linked in conditionally, based on build-time configuration. CAUTION: This
  * plugin should never be part of a _real_ production environment.
  */
object Debug extends BackendPlugin:
  type Requires = SessionStore

  override val id = "debug"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.AuthenticatedAndAdminPassword
  override val routes: Routes[Requires & RequestContext, Nothing] =
    Routes(Method.GET / "debug" -> handler(response))

  private[debug] def response: UIO[Response] =
    collect(Seq.empty).map(signature => Response.text(signature).addHeader(Header.CacheControl.NoStore))

  private final case class NginxProcess(
      pid: Long,
      command: Option[String],
      arguments: Seq[String],
      commandLine: Option[String]
  )

  private[debug] def collect(applicationArguments: Seq[String]): UIO[String] =
    ZIO
      .foreach(
        Seq(
          "Operating system" -> operatingSystem,
          "Disk space" -> diskSpace,
          "Memory" -> memory,
          "Environment variables" -> environmentVariables,
          "Backend command-line arguments" -> ZIO.succeed(commandLineArguments(applicationArguments)),
          "Java runtime" -> javaRuntime,
          "nginx reverse proxy" -> nginx
        )
      )((title, body) => section(title, body))
      .map(_.mkString("\n\n") + "\n")

  private def section(title: String, body: Task[String]): UIO[String] =
    body.fold(
      error => s"=== $title ===\nUnavailable: ${describe(error)}",
      content => s"=== $title ===\n$content"
    )

  private def operatingSystem: Task[String] =
    for
      name <- systemProperty("os.name")
      version <- systemProperty("os.version")
      architecture <- systemProperty("os.arch")
      processors <- ZIO.attempt(Runtime.getRuntime.availableProcessors().toString)
      linuxDetails <-
        if name.toLowerCase.contains("linux") then linuxOperatingSystemDetails(version)
        else ZIO.succeed(Seq.empty)
      properties = Seq(
        "Name" -> name,
        "Version" -> version,
        "Architecture" -> architecture,
        "Processors" -> processors
      )
    yield (properties ++ linuxDetails).map((key, value) => s"$key: $value").mkString("\n")

  private def linuxOperatingSystemDetails(osVersion: String): UIO[Seq[(String, String)]] =
    for
      osRelease <- readFileOption(Paths.get("/etc/os-release"))
      kernelRelease <- readFileOption(Paths.get("/proc/sys/kernel/osrelease"))
      procVersion <- readFileOption(Paths.get("/proc/version"))
      distribution = osRelease.flatMap: contents =>
        osReleaseValue(contents, "PRETTY_NAME").orElse(osReleaseValue(contents, "NAME"))
      kernel = kernelRelease
        .map(_.trim)
        .filter(_.nonEmpty)
        .orElse(procVersion.map(_.trim).filter(_.nonEmpty))
        .orElse(Option(osVersion).filter(_.nonEmpty))
    yield Seq("Distribution" -> distribution.getOrElse("Unavailable"), "Kernel" -> kernel.getOrElse("Unavailable"))

  private def diskSpace: Task[String] =
    ZIO.attemptBlocking:
      Option(File.listRoots()).toSeq.flatten match
        case Seq() => "Unavailable: no filesystem roots were reported"
        case roots =>
          roots
            .sortBy(_.getAbsolutePath)
            .map: root =>
              val total = root.getTotalSpace
              val free = root.getFreeSpace
              val usable = root.getUsableSpace
              Seq(
                s"Filesystem: ${root.getAbsolutePath}",
                s"  Total: ${formatBytes(total)} ($total bytes)",
                s"  Free: ${formatBytes(free)} ($free bytes)",
                s"  Usable by this process: ${formatBytes(usable)} ($usable bytes)"
              ).mkString("\n")
            .mkString("\n")

  private def memory: Task[String] =
    ZIO.attempt:
      val runtime = Runtime.getRuntime
      val jvmTotal = runtime.totalMemory()
      val jvmFree = runtime.freeMemory()
      val jvmUsed = jvmTotal - jvmFree
      val jvm = Seq(
        s"JVM maximum heap: ${formatBytes(runtime.maxMemory())} (${runtime.maxMemory()} bytes)",
        s"JVM allocated heap: ${formatBytes(jvmTotal)} ($jvmTotal bytes)",
        s"JVM used heap: ${formatBytes(jvmUsed)} ($jvmUsed bytes)",
        s"JVM free in allocated heap: ${formatBytes(jvmFree)} ($jvmFree bytes)"
      )

      val physical = ManagementFactory.getOperatingSystemMXBean match
        case bean: com.sun.management.OperatingSystemMXBean =>
          val totalMemory = bean.getTotalMemorySize
          val freeMemory = bean.getFreeMemorySize
          val totalSwap = bean.getTotalSwapSpaceSize
          val freeSwap = bean.getFreeSwapSpaceSize
          Seq(
            s"Physical memory total: ${formatBytes(totalMemory)} ($totalMemory bytes)",
            s"Physical memory free: ${formatBytes(freeMemory)} ($freeMemory bytes)",
            s"Swap total: ${formatBytes(totalSwap)} ($totalSwap bytes)",
            s"Swap free: ${formatBytes(freeSwap)} ($freeSwap bytes)"
          )
        case _ => Seq("Physical memory: Unavailable from this JVM")

      (physical ++ jvm).mkString("\n")

  private def environmentVariables: Task[String] =
    ZIO.attempt:
      java.lang.System
        .getenv()
        .asScala
        .toSeq
        .sortBy(_._1)
        .map((key, value) => s"$key=${escape(value)}")
        .mkString("\n") match
        case ""     => "(none)"
        case values => values

  private def commandLineArguments(arguments: Seq[String]): String =
    if arguments.isEmpty then "(none)"
    else arguments.zipWithIndex.map((argument, index) => s"[$index]=${escape(argument)}").mkString("\n")

  private def javaRuntime: Task[String] =
    for
      javaVersion <- systemProperty("java.version")
      runtimeName <- systemProperty("java.runtime.name")
      runtimeVersion <- systemProperty("java.runtime.version")
      vmName <- systemProperty("java.vm.name")
      vmVersion <- systemProperty("java.vm.version")
      vendor <- systemProperty("java.vendor")
      javaHome <- systemProperty("java.home")
      inputArguments <- ZIO.attempt:
        ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.map(escape).mkString(" ") match
          case ""   => "(none)"
          case args => args
    yield Seq(
      s"Java version: $javaVersion",
      s"Runtime: $runtimeName $runtimeVersion",
      s"VM: $vmName $vmVersion",
      s"Vendor: $vendor",
      s"Java home: $javaHome",
      s"JVM input arguments: $inputArguments"
    ).mkString("\n")

  private def nginx: Task[String] =
    findNginxProcesses.flatMap:
      case Seq()     => ZIO.succeed("Running: no\nReverse proxy detected: no\nConfiguration: not attempted")
      case processes =>
        val processSummary = processes.map: process =>
          val details = process.commandLine.orElse(process.command).getOrElse("command unavailable")
          s"  PID ${process.pid}: $details"
        extractNginxConfiguration(processes.head).foldZIO(
          reason =>
            ZIO.succeed:
              Seq(
                "Running: yes",
                "Reverse proxy detected: unknown (configuration could not be inspected)",
                "Processes:",
                processSummary.mkString("\n"),
                s"Configuration extraction: failed ($reason)"
              ).mkString("\n")
          ,
          (source, configuration) =>
            ZIO.succeed:
              val reverseProxy = containsReverseProxyDirective(configuration)
              val configOutput =
                if reverseProxy then s"\n\n--- nginx configuration ($source) ---\n$configuration"
                else ""
              Seq(
                "Running: yes",
                s"Reverse proxy detected: ${if reverseProxy then "yes" else "no"}",
                "Processes:",
                processSummary.mkString("\n"),
                s"Configuration extraction: succeeded ($source)"
              ).mkString("\n") + configOutput
        )

  private def findNginxProcesses: Task[Seq[NginxProcess]] =
    ZIO.scoped:
      ZIO
        .acquireRelease(ZIO.attemptBlocking(ProcessHandle.allProcesses())): handles =>
          ZIO.attemptBlocking(handles.close()).ignore
        .flatMap: handles =>
          for
            processHandles <- ZIO.attemptBlocking(handles.iterator().asScala.toSeq)
            processes <- ZIO.foreach(processHandles)(inspectNginxProcess)
          yield processes.flatten.sortBy(_.pid)

  private def inspectNginxProcess(handle: ProcessHandle): Task[Option[NginxProcess]] =
    ZIO
      .attemptBlocking:
        val info = handle.info()
        val command = optional(info.command())
        val arguments = optional(info.arguments()).map(_.toSeq).getOrElse(Seq.empty)
        val commandLine = optional(info.commandLine())
        (command, arguments, commandLine, command.flatMap(fileName))
      .flatMap: (command, arguments, commandLine, commandName) =>
        val processName = commandName match
          case some @ Some(_) => ZIO.succeed(some)
          case None           => readFileOption(Paths.get(s"/proc/${handle.pid()}/comm")).map(_.map(_.trim))
        processName.map: name =>
          Option.when(name.exists(value => value.equalsIgnoreCase("nginx") || value.equalsIgnoreCase("nginx.exe"))):
            NginxProcess(handle.pid(), command, arguments, commandLine)

  private def extractNginxConfiguration(process: NginxProcess): IO[String, (String, String)] =
    val commandAttempt = ZIO
      .fromOption(process.command)
      .orElseFail("nginx executable path is unavailable")
      .flatMap(executable => dumpNginxConfiguration(executable, nginxConfigArguments(process.arguments)))

    commandAttempt.orElse:
      for
        candidate <- blocking:
          nginxConfigCandidates(process).find(path => Files.isRegularFile(path) && Files.isReadable(path))
        path <- ZIO.fromOption(candidate).orElseFail("nginx -T failed and no readable nginx.conf was found")
        configuration <- readFile(path).mapError(error => s"could not read ${path.toAbsolutePath}: ${describe(error)}")
      yield path.toAbsolutePath.toString -> configuration

  private def dumpNginxConfiguration(executable: String, configArguments: Seq[String]): IO[String, (String, String)] =
    ZIO.scoped:
      for
        outputFile <- ZIO.acquireRelease(blocking(Files.createTempFile("webapptemplate-nginx-", ".txt")))(deleteFile)
        process <- ZIO.acquireRelease(
          blocking:
            val command = Seq(executable, "-T") ++ configArguments
            new ProcessBuilder(command*)
              .redirectErrorStream(true)
              .redirectOutput(outputFile.toFile)
              .start()
        )(stopProcess)
        completed <- blocking(process.waitFor(5, TimeUnit.SECONDS))
        _ <- if completed then ZIO.unit else ZIO.fail("nginx -T timed out after 5 seconds")
        output <- readFile(outputFile).mapError(describe).map(_.trim)
        exitCode <- blocking(process.exitValue())
        result <-
          if exitCode == 0 && output.nonEmpty then ZIO.succeed("nginx -T" -> output)
          else ZIO.fail(s"nginx -T exited with status $exitCode: ${firstLine(output)}")
      yield result

  private def deleteFile(path: Path): UIO[Unit] =
    ZIO.attemptBlocking(Files.deleteIfExists(path)).unit.ignore

  private def stopProcess(process: Process): UIO[Unit] =
    ZIO
      .attemptBlocking:
        if process.isAlive then
          val _ = process.destroyForcibly()
      .ignore

  private def nginxConfigArguments(arguments: Seq[String]): Seq[String] =
    Seq("-p" -> argumentOption(arguments, "-p"), "-c" -> argumentOption(arguments, "-c")).flatMap:
      case (name, value) => value.toSeq.flatMap(value => Seq(name, value))

  private def nginxConfigCandidates(process: NginxProcess): Seq[Path] =
    val prefix = argumentOption(process.arguments, "-p").map(Paths.get(_))
    val configured = argumentOption(process.arguments, "-c")
      .map(Paths.get(_))
      .map: path =>
        if path.isAbsolute then path else prefix.map(_.resolve(path)).getOrElse(path)
    val besideExecutable =
      process.command.flatMap(command => Option(Paths.get(command).getParent)).map(_.resolve("conf/nginx.conf"))
    (configured.toSeq ++ besideExecutable.toSeq ++ Seq(
      Paths.get("/etc/nginx/nginx.conf"),
      Paths.get("/usr/local/nginx/conf/nginx.conf"),
      Paths.get("/opt/homebrew/etc/nginx/nginx.conf")
    )).distinct

  private def argumentOption(arguments: Seq[String], name: String): Option[String] =
    arguments.zipWithIndex.collectFirst:
      case (argument, index) if argument == name && index + 1 < arguments.length       => arguments(index + 1)
      case (argument, _) if argument.startsWith(name) && argument.length > name.length => argument.drop(name.length)

  private def containsReverseProxyDirective(configuration: String): Boolean =
    "(?im)^\\s*(proxy_pass|fastcgi_pass|uwsgi_pass|scgi_pass|grpc_pass)\\s+".r.findFirstIn(configuration).nonEmpty

  private def optional[A](value: java.util.Optional[A]): Option[A] =
    if value.isPresent then Some(value.get()) else None

  private def fileName(path: String): Option[String] = Option(Paths.get(path).getFileName).map(_.toString)

  private def osReleaseValue(contents: String, key: String): Option[String] =
    contents.linesIterator
      .find(_.startsWith(s"$key="))
      .map(_.drop(key.length + 1).stripPrefix("\"").stripSuffix("\""))

  private def readFile(path: Path): Task[String] =
    ZIO.attemptBlockingIO(Files.readString(path, StandardCharsets.UTF_8))

  private def readFileOption(path: Path): UIO[Option[String]] = readFile(path).option

  private def systemProperty(name: String): Task[String] =
    ZIO.attempt(Option(java.lang.System.getProperty(name)).getOrElse("Unavailable"))

  private def blocking[A](effect: => A): IO[String, A] =
    ZIO.attemptBlocking(effect).mapError(describe)

  private def formatBytes(bytes: Long): String =
    val units = Seq("B", "KiB", "MiB", "GiB", "TiB", "PiB")
    if bytes < 1024 then s"$bytes B"
    else
      var value = bytes.toDouble
      var unit = 0
      while value >= 1024 && unit < units.length - 1 do
        value /= 1024
        unit += 1
      f"$value%.2f ${units(unit)}"

  private def escape(value: String): String =
    value.flatMap:
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case char => char.toString

  private def firstLine(value: String): String =
    value.linesIterator.nextOption().filter(_.nonEmpty).getOrElse("no output")

  private def describe(error: Throwable): String =
    Option(error.getMessage)
      .filter(_.nonEmpty)
      .fold(error.getClass.getSimpleName)(message => s"${error.getClass.getSimpleName}: $message")
