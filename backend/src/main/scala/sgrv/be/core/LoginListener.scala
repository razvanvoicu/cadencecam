package sgrv.be.core

import java.time.Instant
import sgrv.be.auth.SessionUser
import zio.{Task, UIO, ZEnvironment, ZIO}

/** A login that has just completed: the browser session exists in the store and its cookie is about to be issued.
  *
  * `sessionKey` is the opaque browser-session secret. It is supplied because it is the only stable identifier of one
  * login, which lets a listener key its own records per login. Treat it as a secret: derive from it rather than copying
  * it into another collection.
  */
final case class LoginEvent(sessionKey: String, user: SessionUser, at: Instant)

/** Nominal contract for code that must run when a login succeeds, discovered exactly like [[BackendPlugin]].
  *
  * This is the counterpart to a route plugin: instead of contributing an endpoint the browser calls, it reacts to a
  * lifecycle event the host raises. Its capability requirements are declared and resolved the same way, so a listener
  * can use host services the login flow itself knows nothing about.
  *
  * A listener is isolated: a failure is logged and the login still succeeds. Authentication does not depend on whatever
  * a listener wanted to do about it.
  */
trait LoginListener:
  type Requires

  def id: String
  def apiVersion: Int = LoginListener.ApiVersion
  def requirements: CapabilitySet[Requires]
  def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit]

object LoginListener:
  val ApiVersion = 1

/** Host service that fans a completed login out to every activated listener. Supplied to [[sgrv.be.auth.Callback]] as
  * the `login-notifier` capability so the login flow needs no knowledge of who is listening.
  */
trait LoginNotifier:
  def loginSucceeded(event: LoginEvent): UIO[Unit]

private[be] object LoginNotifier:
  def loginSucceeded(event: LoginEvent): ZIO[LoginNotifier, Nothing, Unit] =
    ZIO.serviceWithZIO[LoginNotifier](_.loginSucceeded(event))

  val none: LoginNotifier = _ => ZIO.unit

private[be] enum ListenerStatus:
  case Active(id: String, className: String, dispatch: LoginEvent => UIO[Unit])
  case Skipped(id: String, className: String, missing: zio.Chunk[MissingCapability])
  case Rejected(className: String, reason: String)

private[be] object LoginListeners:
  private val validListenerId = "[a-z][a-z0-9]*(?:[-.][a-z0-9]+)*".r

  /** Discovers the listeners, resolves each one's capabilities against the host, and returns a notifier closed over
    * them. Listeners run in id order so their relative ordering is deterministic rather than classpath-dependent.
    */
  def notifier(
      registry: CapabilityRegistry,
      classLoader: ClassLoader = getClass.getClassLoader
  ): Task[LoginNotifier] =
    discover(registry, classLoader).flatMap: statuses =>
      ZIO
        .foreachDiscard(statuses):
          case ListenerStatus.Active(id, _, _)        => ZIO.logInfo(s"Activated login listener $id")
          case ListenerStatus.Skipped(id, _, missing) =>
            ZIO.logWarning(s"Skipped login listener $id; missing capabilities: ${missing.map(_.id).mkString(", ")}")
          case ListenerStatus.Rejected(className, reason) =>
            ZIO.logWarning(s"Rejected login listener $className: $reason")
        .as(fromStatuses(statuses))

  private[be] def discover(
      registry: CapabilityRegistry,
      classLoader: ClassLoader = getClass.getClassLoader
  ): Task[Seq[ListenerStatus]] =
    ModuleDiscovery
      .implementations(classOf[LoginListener], classLoader)
      .flatMap: classNames =>
        ZIO.foreach(classNames): className =>
          ModuleDiscovery
            .load(className, classOf[LoginListener], classLoader)
            .fold(
              error => ListenerStatus.Rejected(className, describe(error)),
              listener => activate(listener, className, registry)
            )

  private[be] def activate(
      listener: LoginListener,
      className: String,
      registry: CapabilityRegistry
  ): ListenerStatus =
    val listenerId = Option(listener.id).map(_.trim).getOrElse("")
    if !validListenerId.matches(listenerId) then
      ListenerStatus.Rejected(className, s"Invalid login listener id '${listener.id}'")
    else if listener.apiVersion != LoginListener.ApiVersion then
      ListenerStatus.Rejected(
        className,
        s"Login listener $listenerId uses API version ${listener.apiVersion}; " +
          s"host provides ${LoginListener.ApiVersion}"
      )
    else
      listener.requirements.resolve(registry) match
        case Left(missing)      => ListenerStatus.Skipped(listenerId, className, missing)
        case Right(environment) => ListenerStatus.Active(listenerId, className, close(listener, environment))

  /** Closes a listener over its resolved environment and swallows its failures, so one listener can neither break a
    * login nor prevent the listeners after it from running.
    */
  private def close(
      listener: LoginListener,
      environment: ZEnvironment[listener.Requires]
  ): LoginEvent => UIO[Unit] =
    event =>
      listener
        .onLogin(event)
        .provideEnvironment(environment)
        .catchAllCause: cause =>
          ZIO.logErrorCause(s"Login listener ${listener.id} failed; the login itself still succeeded", cause)

  private[be] def fromStatuses(statuses: Seq[ListenerStatus]): LoginNotifier =
    val active = statuses.collect { case listener: ListenerStatus.Active => listener }.sortBy(_.id)
    event => ZIO.foreachDiscard(active)(_.dispatch(event))

  private def describe(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
