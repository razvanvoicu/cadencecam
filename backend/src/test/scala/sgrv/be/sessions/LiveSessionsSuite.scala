package sgrv.be.sessions

import sgrv.api.{LiveCommand, LiveReading, LiveState}
import zio.*
import zio.http.{WebSocketChannel, WebSocketChannelEvent, WebSocketFrame}
import zio.http.ChannelEvent.Read
import zio.json.*

class LiveSessionsSuite extends munit.FunSuite:

  private def run[A](effect: ZIO[Any, Any, A]): A =
    Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(effect.mapError(error => RuntimeException(error.toString))).getOrThrowFiberFailure()
    }

  /** A channel that remembers what it was told, so a room's routing can be asserted rather than inferred. */
  private final class Recorder extends WebSocketChannel:
    val sent = java.util.concurrent.ConcurrentLinkedQueue[String]()
    @volatile var closed = false

    def texts: List[String] = sent.toArray.toList.map(_.asInstanceOf[String])

    override def awaitShutdown(implicit trace: Trace): UIO[Unit] = ZIO.unit
    override def receive(implicit trace: Trace): Task[WebSocketChannelEvent] = ZIO.never
    override def receiveAll[Env, Err](f: WebSocketChannelEvent => ZIO[Env, Err, Any])(implicit
        trace: Trace
    ): ZIO[Env, Err, Unit] = ZIO.never
    override def send(in: WebSocketChannelEvent)(implicit trace: Trace): Task[Unit] =
      ZIO.succeed:
        in match
          case Read(WebSocketFrame.Text(text)) => val _ = sent.add(text)
          case _                               => ()
    override def sendAll(in: Iterable[WebSocketChannelEvent])(implicit trace: Trace): Task[Unit] =
      ZIO.foreachDiscard(in)(send)
    override def shutdown(implicit trace: Trace): UIO[Unit] = ZIO.succeed { closed = true }

  /** A fresh account per test, since the relay is deliberately process-wide. */
  private def account(name: String) = s"$name-${java.util.UUID.randomUUID()}@example.com"

  private val reading = LiveReading(12, 30.0, "Counting Q4+Q2 · 1.0s/rep")

  test("a reading reaches every device watching that account"):
    val email = account("watched")
    val first = Recorder()
    val second = Recorder()

    run:
      for
        _ <- LiveSessions.watcherJoined(email, "a", first)
        _ <- LiveSessions.watcherJoined(email, "b", second)
        _ <- LiveSessions.publish(email, reading)
      yield ()

    val expected = LiveState(acquiring = false, Some(reading)).toJson
    assertEquals(first.texts, List(expected))
    assertEquals(second.texts, List(expected))

  test("a reading reaches nobody else's dashboard"):
    val mine = account("mine")
    val theirs = account("theirs")
    val watcher = Recorder()

    run:
      for
        _ <- LiveSessions.watcherJoined(theirs, "a", watcher)
        _ <- LiveSessions.publish(mine, reading)
      yield ()

    assertEquals(watcher.texts, Nil)

  test("a dashboard opened mid-set is told the count at once"):
    // Otherwise it shows nothing until the next rep, which on a slow exercise looks like a broken screen.
    val email = account("late")
    val latecomer = Recorder()

    val greeted = run:
      for
        // The real sequence: a device is counting, and a dashboard is opened partway through.
        _ <- LiveSessions.acquirerJoined(email, "counting", Recorder())
        _ <- LiveSessions.publish(email, reading)
        latest <- LiveSessions.watcherJoined(email, "late", latecomer)
      yield latest

    assertEquals(greeted, LiveState(acquiring = true, Some(reading)))

  test("an account with nothing counting remembers nothing to greet a dashboard with"):
    // A room is discarded once its last device leaves, so a dashboard opened afterwards is told nothing rather than
    // being shown a count from a session that ended.
    val email = account("finished")

    val greeted = run:
      for
        _ <- LiveSessions.acquirerJoined(email, "counting", Recorder())
        _ <- LiveSessions.publish(email, reading)
        _ <- LiveSessions.acquirerLeft(email, "counting")
        latest <- LiveSessions.watcherJoined(email, "afterwards", Recorder())
      yield latest

    assertEquals(greeted, LiveState(acquiring = false, None))

  test("a command reaches this account's acquirer"):
    val email = account("commanded")
    val acquirer = Recorder()

    val delivered = run:
      for
        _ <- LiveSessions.acquirerJoined(email, "one", acquirer)
        sent <- LiveSessions.command(email, LiveCommand.Reset)
      yield sent

    assert(delivered)
    assertEquals(acquirer.texts, List(LiveCommand.Reset.toJson))

  test("a command with no acquirer to receive it says so rather than pretending"):
    val delivered = run(LiveSessions.command(account("alone"), LiveCommand.CaptureTrace))

    assert(!delivered)

  test("a second acquirer displaces the first, which is closed rather than left reporting"):
    // A phone whose socket died without a close frame would otherwise lock its own account out, and the newest
    // connection is the one the user is looking at.
    val email = account("displaced")
    val stale = Recorder()
    val fresh = Recorder()

    val displaced = run:
      for
        _ <- LiveSessions.acquirerJoined(email, "old", stale)
        replaced <- LiveSessions.acquirerJoined(email, "new", fresh)
        _ <- ZIO.foreachDiscard(replaced)(_.shutdown)
        _ <- LiveSessions.command(email, LiveCommand.Reset)
      yield replaced

    assert(displaced.isDefined, "the earlier acquirer was not handed back to be closed")
    assert(stale.closed, "the earlier acquirer was left open")
    // The command went to the new one only.
    assertEquals(fresh.texts, List(LiveCommand.Reset.toJson))
    assertEquals(stale.texts, Nil)

  test("a departing acquirer removes itself, not whichever one is registered"):
    val email = account("departing")
    val stale = Recorder()
    val fresh = Recorder()

    val stillThere = run:
      for
        _ <- LiveSessions.acquirerJoined(email, "old", stale)
        _ <- LiveSessions.acquirerJoined(email, "new", fresh)
        // The displaced connection closes and reports its departure afterwards, which must not unregister the one
        // that replaced it.
        _ <- LiveSessions.acquirerLeft(email, "old")
        present <- LiveSessions.hasAcquirer(email)
      yield present

    assert(stillThere, "a late departure from the displaced connection unregistered the current one")

  test("the last device leaving takes the room with it"):
    val email = account("emptying")

    val present = run:
      for
        _ <- LiveSessions.acquirerJoined(email, "one", Recorder())
        _ <- LiveSessions.watcherJoined(email, "two", Recorder())
        _ <- LiveSessions.acquirerLeft(email, "one")
        _ <- LiveSessions.watcherLeft(email, "two")
        // Nothing is remembered for an account that has gone home: a later reading has nowhere to go.
        stillThere <- LiveSessions.hasAcquirer(email)
        latest <- LiveSessions.watcherJoined(email, "three", Recorder())
      yield (stillThere, latest)

    assertEquals(present, (false, LiveState(acquiring = false, None)))

  test("connection ids are distinct, which is what lets one socket leave without taking another with it"):
    val ids = Seq.fill(50)(LiveRelay.connectionId())

    assertEquals(ids.distinct.size, 50)
    assert(ids.forall(id => id.length == 16 && id.forall(c => c.isDigit || ('a' to 'f').contains(c))), ids.head)

  test("a dashboard is told the moment something starts counting, without waiting for a rep"):
    // Otherwise the screen goes on saying nothing is counting while the other phone plainly is, and the first sign
    // of life is a rep that may be twenty seconds away.
    val email = account("announced")
    val watcher = Recorder()

    run:
      for
        _ <- LiveSessions.watcherJoined(email, "watching", watcher)
        _ <- LiveSessions.acquirerJoined(email, "counting", Recorder())
      yield ()

    assertEquals(watcher.texts.last.fromJson[LiveState], Right(LiveState(acquiring = true, None)))

  test("a dashboard is told when the counting device goes away"):
    val email = account("departed")
    val watcher = Recorder()

    run:
      for
        _ <- LiveSessions.acquirerJoined(email, "counting", Recorder())
        _ <- LiveSessions.watcherJoined(email, "watching", watcher)
        _ <- LiveSessions.publish(email, reading)
        _ <- LiveSessions.acquirerLeft(email, "counting")
      yield ()

    assertEquals(watcher.texts.last.fromJson[LiveState], Right(LiveState(acquiring = false, Some(reading))))

