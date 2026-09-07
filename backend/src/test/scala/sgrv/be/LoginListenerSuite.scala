package sgrv.be

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import sgrv.be.auth.SessionUser
import sgrv.be.core.*
import zio.*

trait TestRecorder:
  def record(value: String): UIO[Unit]

object RecordingListener extends LoginListener:
  type Requires = TestRecorder

  override val id = "test-recording"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.one(Capability[TestRecorder]("test-recorder"))
  override def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit] =
    ZIO.serviceWithZIO[TestRecorder](_.record(s"recorded:${event.user.email}"))

object FailingListener extends LoginListener:
  type Requires = Any

  override val id = "test-failing"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
  override def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit] =
    ZIO.fail(new IllegalStateException("listener exploded"))

object IncompatibleListener extends LoginListener:
  type Requires = Any

  override val id = "test-incompatible"
  override val apiVersion = LoginListener.ApiVersion + 1
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
  override def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit] = ZIO.unit

object BadlyNamedListener extends LoginListener:
  type Requires = Any

  override val id = "Not A Valid Id"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.empty
  override def onLogin(event: LoginEvent): ZIO[Requires, Throwable, Unit] = ZIO.unit

class LoginListenerSuite extends munit.FunSuite:

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
    val status = LoginListeners.activate(RecordingListener, RecordingListener.getClass.getName, registry)

    run(LoginListeners.fromStatuses(Seq(status)).loginSucceeded(event))

    assertEquals(sink.get(), List("recorded:jane@example.com"))

  test("skips a listener whose capabilities are missing, naming them"):
    LoginListeners.activate(RecordingListener, RecordingListener.getClass.getName, CapabilityRegistry.empty) match
      case ListenerStatus.Skipped(id, _, missing) =>
        assertEquals(id, "test-recording")
        assertEquals(missing.map(_.id), Chunk("test-recorder"))
      case other => fail(s"Expected a skip, got $other")

  test("rejects an invalid id and an incompatible listener API"):
    val badId = LoginListeners.activate(BadlyNamedListener, "BadlyNamedListener", CapabilityRegistry.empty)
    val incompatible = LoginListeners.activate(IncompatibleListener, "IncompatibleListener", CapabilityRegistry.empty)

    assert(badId.isInstanceOf[ListenerStatus.Rejected], s"Expected a rejection, got $badId")
    assert(incompatible.isInstanceOf[ListenerStatus.Rejected], s"Expected a rejection, got $incompatible")

  test("a failing listener neither fails the login nor stops the listeners after it"):
    val sink = new AtomicReference(List.empty[String])
    val registry = CapabilityRegistry.fromEnvironment(ZEnvironment(recorder(sink)))
    val statuses = Seq(
      LoginListeners.activate(FailingListener, FailingListener.getClass.getName, registry),
      LoginListeners.activate(RecordingListener, RecordingListener.getClass.getName, registry)
    )

    // Would raise if the notifier propagated the failure rather than isolating it.
    run(LoginListeners.fromStatuses(statuses).loginSucceeded(event))

    assertEquals(sink.get(), List("recorded:jane@example.com"))

  test("discovers the counting-session listener on the real classpath"):
    val discovered = run(LoginListeners.discover(CapabilityRegistry.empty))
    val ids = discovered.collect:
      case ListenerStatus.Active(id, _, _)  => id
      case ListenerStatus.Skipped(id, _, _) => id

    assert(ids.contains("counting-session"), s"Expected the counting-session listener among $ids")
