package sgrv.be.core

import zio.{Task, UIO, ZEnvironment, ZIO}
import zio.json.ast.Json

/** Nominal contract for code that adds application data to the `/me` response, discovered exactly like
  * [[BackendPlugin]] and [[SessionListener]].
  *
  * `/me` is the one call every page load already makes, so anything the frontend needs about the current session can
  * ride along on it instead of costing a second round trip. Contributions are namespaced under the contributor's id so
  * two of them can never collide, and so the template's own payload keeps its shape.
  *
  * A contributor is isolated: a failure is logged and its key is simply absent. Resolving who is signed in must not
  * depend on whatever an application wanted to say about them.
  */
trait CurrentUserContributor:
  type Requires

  def id: String
  def apiVersion: Int = CurrentUserContributor.ApiVersion
  def requirements: CapabilitySet[Requires]

  /** Returns this contributor's entry, or `None` to contribute nothing for this request. */
  def contribute(context: RequestContext.Authenticated): ZIO[Requires, Throwable, Option[Json]]

object CurrentUserContributor:
  val ApiVersion = 1

/** Host service that collects every activated contributor's entry for one authenticated request. Supplied to
  * [[sgrv.be.auth.Me]] as the `current-user-details` capability so `/me` needs no knowledge of its contributors.
  */
trait CurrentUserDetails:
  def extras(context: RequestContext.Authenticated): UIO[Map[String, Json]]

private[be] object CurrentUserDetails:
  def extras(context: RequestContext.Authenticated): ZIO[CurrentUserDetails, Nothing, Map[String, Json]] =
    ZIO.serviceWithZIO[CurrentUserDetails](_.extras(context))

  val none: CurrentUserDetails = _ => ZIO.succeed(Map.empty)

private[be] enum ContributorStatus:
  case Active(id: String, className: String, collect: RequestContext.Authenticated => UIO[Option[Json]])
  case Skipped(id: String, className: String, missing: zio.Chunk[MissingCapability])
  case Rejected(className: String, reason: String)

private[be] object CurrentUserContributors:
  private val validContributorId = "[a-z][a-z0-9]*(?:[-.][a-z0-9]+)*".r

  def details(
      registry: CapabilityRegistry,
      classLoader: ClassLoader = getClass.getClassLoader
  ): Task[CurrentUserDetails] =
    discover(registry, classLoader).flatMap: statuses =>
      ZIO
        .foreachDiscard(statuses):
          case ContributorStatus.Active(id, _, _)        => ZIO.logInfo(s"Activated current-user contributor $id")
          case ContributorStatus.Skipped(id, _, missing) =>
            ZIO.logWarning(
              s"Skipped current-user contributor $id; missing capabilities: ${missing.map(_.id).mkString(", ")}"
            )
          case ContributorStatus.Rejected(className, reason) =>
            ZIO.logWarning(s"Rejected current-user contributor $className: $reason")
        .as(fromStatuses(statuses))

  private[be] def discover(
      registry: CapabilityRegistry,
      classLoader: ClassLoader = getClass.getClassLoader
  ): Task[Seq[ContributorStatus]] =
    ModuleDiscovery
      .implementations(classOf[CurrentUserContributor], classLoader)
      .flatMap: classNames =>
        ZIO.foreach(classNames): className =>
          ModuleDiscovery
            .load(className, classOf[CurrentUserContributor], classLoader)
            .fold(
              error => ContributorStatus.Rejected(className, describe(error)),
              contributor => activate(contributor, className, registry)
            )

  private[be] def activate(
      contributor: CurrentUserContributor,
      className: String,
      registry: CapabilityRegistry
  ): ContributorStatus =
    val contributorId = Option(contributor.id).map(_.trim).getOrElse("")
    if !validContributorId.matches(contributorId) then
      ContributorStatus.Rejected(className, s"Invalid current-user contributor id '${contributor.id}'")
    else if contributor.apiVersion != CurrentUserContributor.ApiVersion then
      ContributorStatus.Rejected(
        className,
        s"Current-user contributor $contributorId uses API version ${contributor.apiVersion}; " +
          s"host provides ${CurrentUserContributor.ApiVersion}"
      )
    else
      contributor.requirements.resolve(registry) match
        case Left(missing)      => ContributorStatus.Skipped(contributorId, className, missing)
        case Right(environment) => ContributorStatus.Active(contributorId, className, close(contributor, environment))

  /** Closes a contributor over its resolved environment and swallows its failures, so one contributor can neither break
    * `/me` nor suppress the contributors after it.
    */
  private def close(
      contributor: CurrentUserContributor,
      environment: ZEnvironment[contributor.Requires]
  ): RequestContext.Authenticated => UIO[Option[Json]] =
    context =>
      contributor
        .contribute(context)
        .provideEnvironment(environment)
        .catchAllCause: cause =>
          ZIO
            .logErrorCause(s"Current-user contributor ${contributor.id} failed; its entry is omitted", cause)
            .as(None)

  private[be] def fromStatuses(statuses: Seq[ContributorStatus]): CurrentUserDetails =
    val active = statuses.collect { case contributor: ContributorStatus.Active => contributor }.sortBy(_.id)
    context =>
      ZIO
        .foreach(active)(contributor => contributor.collect(context).map(_.map(contributor.id -> _)))
        .map(_.flatten.toMap)

  private def describe(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
