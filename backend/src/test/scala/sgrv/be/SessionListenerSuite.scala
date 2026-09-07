package sgrv.be

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import sgrv.be.auth.SessionUser
import sgrv.be.core.*
import zio.*

trait TestRecorder:
  def record(value: String): UIO[Unit]

object RecordingListener extends SessionListener:
  type Requires = TestRecorder

  override val id = "test-recording"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.one(Capability[TestRecorder]("test-recorder"))
  override def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit] =
    ZIO.serviceWithZIO[TestRecorder](_.record(s"recorded:${event.user.email}"))

object LogoutRecordingListener extends SessionListener:
  type Requires = TestRecorder

  override val id = "test-logout-recording"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.one(Capability[TestRecorder]("test-recorder"))
  override def onLogout(event: LogoutEvent): ZIO[Requires, Throwable, Unit] =
    ZIO.serviceWithZIO[TestRecorder](_.record(s"logged-out:${event.user.email}"))

object FailingListener extends SessionListener:
  type Requires = Any

  override val id = "test-failing"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
  override def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit] =
    ZIO.fail(new IllegalStateException("listener exploded"))
  override def onLogout(event: LogoutEvent): ZIO[Requires, Throwable, Unit] =
    ZIO.fail(new IllegalStateException("listener exploded on logout"))

object IncompatibleListener extends SessionListener:
  type Requires = Any

  override val id = "test-incompatible"
  override val apiVersion = SessionListener.ApiVersion + 1
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
  override def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit] = ZIO.unit

object BadlyNamedListener extends SessionListener:
  type Requires = Any

  override val id = "Not A Valid Id"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
  override def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit] = ZIO.unit

class SessionListenerSuite extends munit.FunSuite:

  private def run[A](effect: ZIO[Any, Any, A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe
        .run(effect.mapError(error => new RuntimeException(error.toString)))
        .getOrThrowFiberFailure()
    }

  private val event = LoginEvent("session-key", SessionUser("jane@example.com", "Jane"), Instant.EPOCH)

  private def recorder(sink: AtomicReference[List[String]]): TestRecorder =
    (value: String) => ZIO.succeed { val _ = sink.updateAndGet(value :: _) }

  test("delivers the login to a listener whose capabilities resolve"):
    val sink = new AtomicReference(List.empty[String])
    val registry = CapabilityRegistry.fromEnvironment(ZEnvironment(recorder(sink)))
    val status = SessionListeners.activate(RecordingListener, RecordingListener.getClass.getName, registry)

    run(SessionListeners.fromStatuses(Seq(status)).loginSucceeded(event))

    assertEquals(sink.get(), List("recorded:jane@example.com"))

  test("skips a listener whose capabilities are missing, naming them"):
    SessionListeners.activate(RecordingListener, RecordingListener.getClass.getName, CapabilityRegistry.empty) match
      case ListenerStatus.Skipped(id, _, missing) =>
        assertEquals(id, "test-recording")
        assertEquals(missing.map(_.id), Chunk("test-recorder"))
      case other => fail(s"Expected a skip, got $other")

  test("rejects an invalid id and an incompatible listener API"):
    val badId = SessionListeners.activate(BadlyNamedListener, "BadlyNamedListener", CapabilityRegistry.empty)
    val incompatible = SessionListeners.activate(IncompatibleListener, "IncompatibleListener", CapabilityRegistry.empty)

    assert(badId.isInstanceOf[ListenerStatus.Rejected], s"Expected a rejection, got $badId")
    assert(incompatible.isInstanceOf[ListenerStatus.Rejected], s"Expected a rejection, got $incompatible")

  test("a failing listener neither fails the login nor stops the listeners after it"):
    val sink = new AtomicReference(List.empty[String])
    val registry = CapabilityRegistry.fromEnvironment(ZEnvironment(recorder(sink)))
    val statuses = Seq(
      SessionListeners.activate(FailingListener, FailingListener.getClass.getName, registry),
      SessionListeners.activate(RecordingListener, RecordingListener.getClass.getName, registry)
    )

    // Would raise if the notifier propagated the failure rather than isolating it.
    run(SessionListeners.fromStatuses(statuses).loginSucceeded(event))

    assertEquals(sink.get(), List("recorded:jane@example.com"))

  test("discovers the counting-session listener on the real classpath"):
    val discovered = run(SessionListeners.discover(CapabilityRegistry.empty))
    val ids = discovered.collect:
      case ListenerStatus.Active(id, _, _)  => id
      case ListenerStatus.Skipped(id, _, _) => id

    assert(ids.contains("counting-session"), s"Expected the counting-session listener among $ids")

  test("logout reaches every listener, and a listener may implement only one half"):
    val sink = new AtomicReference(List.empty[String])
    val registry = CapabilityRegistry.fromEnvironment(ZEnvironment(recorder(sink)))
    val notifier = SessionListeners.fromStatuses(
      Seq(
        SessionListeners.activate(LogoutRecordingListener, "LogoutRecordingListener", registry),
        // Implements onLogin only; its inherited onLogout must be a harmless no-op.
        SessionListeners.activate(RecordingListener, RecordingListener.getClass.getName, registry)
      )
    )

    run(notifier.loggedOut(LogoutEvent("session-key", SessionUser("jane@example.com", "Jane"), Instant.EPOCH)))

    assertEquals(sink.get(), List("logged-out:jane@example.com"))

  test("a failing logout listener does not fail the logout"):
    val statuses =
      Seq(SessionListeners.activate(FailingListener, FailingListener.getClass.getName, CapabilityRegistry.empty))

    // Would raise if the notifier propagated the failure rather than isolating it.
    run(SessionListeners.fromStatuses(statuses).loggedOut(LogoutEvent("k", SessionUser("a@b.c", "A"), Instant.EPOCH)))
