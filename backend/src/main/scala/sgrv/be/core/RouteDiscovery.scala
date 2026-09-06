package sgrv.be.core

import io.github.classgraph.ClassGraph
import scala.jdk.CollectionConverters.*
import zio.{Chunk, Scope, Task, ZEnvironment, ZIO}
import zio.http.{Request, Response, Routes, handler}

private[be] enum PluginStatus:
  case Active(id: String, className: String, routes: Routes[Any, Nothing])
  case Skipped(id: String, className: String, missing: Chunk[MissingCapability])
  case Rejected(className: String, reason: String)
  case Failed(id: String, className: String, cause: Throwable)

private[be] object RouteDiscovery:
  private val validPluginId = "[a-z][a-z0-9]*(?:[-.][a-z0-9]+)*".r

  def discover(
      registry: CapabilityRegistry,
      reservedPatterns: Set[Any] = Set.empty,
      classLoader: ClassLoader = getClass.getClassLoader
  ): Task[Seq[PluginStatus]] =
    scan(classLoader).flatMap: classNames =>
      ZIO
        .foreach(classNames)(loadStatus(_, registry, classLoader))
        .map: loaded =>
          rejectConflicts(rejectDuplicateIds(loaded), reservedPatterns)

  private def scan(classLoader: ClassLoader): Task[Seq[String]] =
    ZIO.scoped:
      for
        result <- ZIO.acquireRelease(
          ZIO.attemptBlocking:
            new ClassGraph()
              .enableClassInfo()
              .acceptPackages("sgrv.be")
              .overrideClassLoaders(classLoader)
              .scan()
        )(scan => ZIO.attemptBlocking(scan.close()).ignore)
        classNames <- ZIO.attemptBlocking:
          result
            .getClassesImplementing(classOf[BackendPlugin].getName)
            .asScala
            .map(_.getName)
            .toSeq
            .sorted
      yield classNames

  private[be] def load(className: String, classLoader: ClassLoader): Task[BackendPlugin] =
    ZIO.attemptBlocking:
      val moduleClass = Class.forName(className.stripSuffix("$") + "$", true, classLoader)
      // noinspection IllegalNull
      moduleClass.getField("MODULE$").get(null) match
        case plugin: BackendPlugin => plugin
        case module                =>
          throw new IllegalArgumentException(
            s"Discovered module ${module.getClass.getName} does not implement ${classOf[BackendPlugin].getName}"
          )

  private def loadStatus(
      className: String,
      registry: CapabilityRegistry,
      classLoader: ClassLoader
  ): Task[PluginStatus] =
    load(className, classLoader).foldZIO(
      error => ZIO.succeed(PluginStatus.Rejected(className, describe(error))),
      plugin =>
        ZIO
          .attempt(activate(plugin, className, registry))
          .fold(
            error => PluginStatus.Failed(safeId(plugin), className, error),
            identity
          )
    )

  private[be] def activate(
      plugin: BackendPlugin,
      className: String,
      registry: CapabilityRegistry
  ): PluginStatus =
    val pluginId = plugin.id.trim
    if !validPluginId.matches(pluginId) then PluginStatus.Rejected(className, s"Invalid plugin id '${plugin.id}'")
    else if plugin.apiVersion != BackendPlugin.ApiVersion then
      PluginStatus.Rejected(
        className,
        s"Plugin $pluginId uses API version ${plugin.apiVersion}; host provides ${BackendPlugin.ApiVersion}"
      )
    else
      plugin.requirements.resolve(registry) match
        case Left(missing)      => PluginStatus.Skipped(pluginId, className, missing)
        case Right(environment) =>
          try PluginStatus.Active(pluginId, className, close(plugin, environment))
          catch case error: Throwable => PluginStatus.Failed(pluginId, className, error)

  private def close(
      plugin: BackendPlugin,
      environment: ZEnvironment[plugin.Requires]
  ): Routes[Any, Nothing] =
    plugin.routes
      .transform[plugin.Requires]: routeHandler =>
        handler: (request: Request) =>
          plugin.accessPolicy
            .authorize(request)
            .flatMap:
              case Left(response) => ZIO.succeed(response)
              case Right(context) =>
                ZIO.scoped:
                  routeHandler(request).provideSomeEnvironment[plugin.Requires & Scope](_.add(context))
      .transform[Any](_.provideEnvironment(environment))

  /**
   * Reject duplicate routes, and routes matching paths of static files.
   */
  private[be] def rejectConflicts(
      statuses: Seq[PluginStatus],
      reservedPatterns: Set[Any] = Set.empty
  ): Seq[PluginStatus] =
    val active = statuses.collect { case plugin: PluginStatus.Active => plugin }
    val owners = active.flatMap: plugin =>
      plugin.routes.routes.map(route => (route.routePattern: Any) -> plugin.id)
    val conflicts = owners
      .groupMap(_._1)(_._2)
      .collect { case (pattern, ids) if ids.size > 1 => pattern -> ids.distinct.sorted }
    val reservedConflicts = active.flatMap: plugin =>
      plugin.routes.routes.map(_.routePattern: Any).filter(reservedPatterns).map(_ -> Seq(plugin.id))
    val allConflicts = conflicts ++ reservedConflicts
    val rejectedIds = allConflicts.values.flatten.toSet

    statuses.map:
      case active: PluginStatus.Active if rejectedIds(active.id) =>
        val details = allConflicts.collect:
          case (pattern, ids) if ids.contains(active.id) => s"$pattern (${ids.mkString(", ")})"
        PluginStatus.Rejected(active.className, s"Route conflict: ${details.toSeq.sorted.mkString("; ")}")
      case status => status

  private[be] def rejectDuplicateIds(statuses: Seq[PluginStatus]): Seq[PluginStatus] =
    val duplicates = statuses
      .collect { case active: PluginStatus.Active => active }
      .groupBy(_.id)
      .collect { case (id, plugins) if plugins.size > 1 => id -> plugins.map(_.className).sorted }

    statuses.map:
      case active: PluginStatus.Active if duplicates.contains(active.id) =>
        PluginStatus.Rejected(
          active.className,
          s"Duplicate plugin id '${active.id}': ${duplicates(active.id).mkString(", ")}"
        )
      case status => status

  /**
   * The route discovery process returns a list of statuses. The actual routes are buried inside
   * the `Active` status objects, and they need to be dug out. Non-active statuses are silently
   * ignored.
   */
  private[be] def fromStatuses(statuses: Seq[PluginStatus]): Routes[Any, Nothing] =
    statuses
      .collect { case PluginStatus.Active(_, _, routes) => routes }
      .foldLeft(Routes.empty: Routes[Any, Nothing])(_ ++ _)

  def routes(
      registry: CapabilityRegistry,
      reservedPatterns: Set[Any] = Set.empty
  ): Task[Routes[Any, Nothing]] =
    discover(registry, reservedPatterns).flatMap: statuses =>
      ZIO
        .foreachDiscard(statuses):
          case PluginStatus.Active(id, _, _)        => ZIO.logInfo(s"Activated backend plugin $id")
          case PluginStatus.Skipped(id, _, missing) =>
            ZIO.logWarning(s"Skipped backend plugin $id; missing capabilities: ${missing.map(_.id).mkString(", ")}")
          case PluginStatus.Rejected(className, reason) =>
            ZIO.logWarning(s"Rejected backend plugin $className: $reason")
          case PluginStatus.Failed(id, className, cause) =>
            ZIO.logErrorCause(s"Backend plugin $id ($className) failed during activation", zio.Cause.fail(cause))
        .as(fromStatuses(statuses))

  private def describe(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)

  private def safeId(plugin: BackendPlugin): String =
    try Option(plugin.id).map(_.trim).filter(_.nonEmpty).getOrElse("<unknown>")
    catch case _: Throwable => "<unknown>"
