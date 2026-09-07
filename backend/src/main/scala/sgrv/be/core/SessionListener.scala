package sgrv.be.core

import java.time.Instant
import scala.annotation.unused
import sgrv.be.auth.SessionUser
import zio.{Task, UIO, ZEnvironment, ZIO}

/** A login that has just completed: the browser session exists in the store and its cookie is about to be issued. */
final case class LoginEvent(sessionKey: String, user: SessionUser, at: Instant)

/** A logout that has just completed: the Google grant is revoked and the browser session is already invalidated. */
final case class LogoutEvent(sessionKey: String, user: SessionUser, at: Instant)

/** Nominal contract for code that reacts to the start and end of a browser session, discovered exactly like
  * [[BackendPlugin]] and [[CurrentUserContributor]].
  *
  * This is the counterpart to a route plugin: instead of contributing an endpoint the browser calls, it reacts to
  * lifecycle events the host raises. Capability requirements are declared and resolved the same way, so a listener can
  * use host services the authentication flow itself knows nothing about.
  *
  * Both hooks default to doing nothing, so a listener implements only the half it cares about.
  *
  * `sessionKey` on either event is the opaque browser-session secret. It is supplied because it is the only stable
  * identifier of one session, which lets a listener key its own records to it. Treat it as a secret: derive from it
  * rather than copying it into another collection.
  *
  * A listener is isolated: a failure is logged, and neither a login nor a logout fails because of it.
  */
trait SessionListener:
  type Requires

  def id: String
  def apiVersion: Int = SessionListener.ApiVersion
  def requirements: CapabilitySet[Requires]

  def onLogin(@unused event: LoginEvent): ZIO[Requires, Throwable, Unit] = ZIO.unit
  def onLogout(@unused event: LogoutEvent): ZIO[Requires, Throwable, Unit] = ZIO.unit

object SessionListener:
  val ApiVersion = 1

/** Host service that fans session lifecycle events out to every activated listener. Supplied to
  * [[sgrv.be.auth.Callback]] and [[sgrv.be.auth.Logout]] as the `session-notifier` capability, so neither flow needs
  * any knowledge of who is listening.
  */
trait SessionNotifier:
  def loginSucceeded(event: LoginEvent): UIO[Unit]
  def loggedOut(event: LogoutEvent): UIO[Unit]

private[be] object SessionNotifier:
  def loginSucceeded(event: LoginEvent): ZIO[SessionNotifier, Nothing, Unit] =
    ZIO.serviceWithZIO[SessionNotifier](_.loginSucceeded(event))

  def loggedOut(event: LogoutEvent): ZIO[SessionNotifier, Nothing, Unit] =
    ZIO.serviceWithZIO[SessionNotifier](_.loggedOut(event))

  val none: SessionNotifier = new SessionNotifier:
    override def loginSucceeded(event: LoginEvent): UIO[Unit] = ZIO.unit
    override def loggedOut(event: LogoutEvent): UIO[Unit] = ZIO.unit

/** A listener closed over its resolved environment, with its failures already isolated. */
private[be] final case class ClosedListener(
    onLogin: LoginEvent => UIO[Unit],
    onLogout: LogoutEvent => UIO[Unit]
)

private[be] enum ListenerStatus:
  case Active(id: String, className: String, closed: ClosedListener)
  case Skipped(id: String, className: String, missing: zio.Chunk[MissingCapability])
  case Rejected(className: String, reason: String)

private[be] object SessionListeners:
  private val validListenerId = "[a-z][a-z0-9]*(?:[-.][a-z0-9]+)*".r

  /** Discovers the listeners, resolves each one's capabilities against the host, and returns a notifier closed over
    * them. Listeners run in id order so their relative ordering is deterministic rather than classpath-dependent.
    */
  def notifier(
      registry: CapabilityRegistry,
      classLoader: ClassLoader = getClass.getClassLoader
  ): Task[SessionNotifier] =
    discover(registry, classLoader).flatMap: statuses =>
      ZIO
        .foreachDiscard(statuses):
          case ListenerStatus.Active(id, _, _)        => ZIO.logInfo(s"Activated session listener $id")
          case ListenerStatus.Skipped(id, _, missing) =>
            ZIO.logWarning(s"Skipped session listener $id; missing capabilities: ${missing.map(_.id).mkString(", ")}")
          case ListenerStatus.Rejected(className, reason) =>
            ZIO.logWarning(s"Rejected session listener $className: $reason")
        .as(fromStatuses(statuses))

  private[be] def discover(
      registry: CapabilityRegistry,
      classLoader: ClassLoader = getClass.getClassLoader
  ): Task[Seq[ListenerStatus]] =
    ModuleDiscovery
      .implementations(classOf[SessionListener], classLoader)
      .flatMap: classNames =>
        ZIO.foreach(classNames): className =>
          ModuleDiscovery
            .load(className, classOf[SessionListener], classLoader)
            .fold(
              error => ListenerStatus.Rejected(className, describe(error)),
              listener => activate(listener, className, registry)
            )

  private[be] def activate(
      listener: SessionListener,
      className: String,
      registry: CapabilityRegistry
  ): ListenerStatus =
    val listenerId = Option(listener.id).map(_.trim).getOrElse("")
    if !validListenerId.matches(listenerId) then
      ListenerStatus.Rejected(className, s"Invalid session listener id '${listener.id}'")
    else if listener.apiVersion != SessionListener.ApiVersion then
      ListenerStatus.Rejected(
        className,
        s"Session listener $listenerId uses API version ${listener.apiVersion}; " +
          s"host provides ${SessionListener.ApiVersion}"
      )
    else
      listener.requirements.resolve(registry) match
        case Left(missing)      => ListenerStatus.Skipped(listenerId, className, missing)
        case Right(environment) => ListenerStatus.Active(listenerId, className, close(listener, environment))

  private def close(
      listener: SessionListener,
      environment: ZEnvironment[listener.Requires]
  ): ClosedListener =
    ClosedListener(
      event => isolate(listener.id, listener.onLogin(event).provideEnvironment(environment)),
      event => isolate(listener.id, listener.onLogout(event).provideEnvironment(environment))
    )

  /** One listener can neither fail the session change that triggered it nor stop the listeners after it. */
  private def isolate(id: String, effect: ZIO[Any, Throwable, Unit]): UIO[Unit] =
    effect.catchAllCause: cause =>
      ZIO.logErrorCause(s"Session listener $id failed; the session change itself still stands", cause)

  private[be] def fromStatuses(statuses: Seq[ListenerStatus]): SessionNotifier =
    val ordered = statuses.collect { case listener: ListenerStatus.Active => listener }.sortBy(_.id).map(_.closed)
    new SessionNotifier:
      override def loginSucceeded(event: LoginEvent): UIO[Unit] = ZIO.foreachDiscard(ordered)(_.onLogin(event))
      override def loggedOut(event: LogoutEvent): UIO[Unit] = ZIO.foreachDiscard(ordered)(_.onLogout(event))

  private def describe(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)
