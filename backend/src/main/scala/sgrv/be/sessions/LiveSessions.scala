package sgrv.be.sessions

import sgrv.api.{LiveCommand, LiveReading, LiveState}
import zio.*
import zio.http.{WebSocketChannel, WebSocketFrame}
import zio.http.ChannelEvent.Read
import zio.json.*

/** One account's live pairing: the device counting, and the devices watching it.
  *
  * Keyed by account rather than by browser session, because the two devices are two separate logins to the same Google
  * account and so have nothing else in common. It is also the shape the acquirer-uniqueness rule will need: one
  * acquirer per account is a property of this room, not something to be enforced elsewhere later.
  *
  * `latest` is kept so a dashboard opened halfway through a set shows the count immediately rather than a blank screen
  * until the next rep.
  */
private[sessions] final case class Room(
    acquirer: Option[(String, WebSocketChannel)] = None,
    watchers: Map[String, WebSocketChannel] = Map.empty,
    latest: Option[LiveReading] = None
)

/** The in-memory relay between an account's acquirer and its dashboards.
  *
  * In memory, and therefore only correct while the service runs as a single instance -- which is why it is pinned to
  * one. Two instances would let a dashboard and its acquirer land on different ones, and the relay would silently
  * deliver nothing. Lifting that pin means giving a dashboard a way to reach the instance holding its acquirer, and
  * this object is where that would be replaced.
  *
  * A singleton rather than a capability: it is process-global by nature, it is application code rather than template
  * code, and giving it a lifetime shorter than the process would be a fiction.
  */
object LiveSessions:
  private val rooms: Ref[Map[String, Room]] =
    Unsafe.unsafe(implicit unsafe => Ref.unsafe.make(Map.empty[String, Room]))

  private def room(email: String): UIO[Room] = rooms.get.map(_.getOrElse(email, Room()))

  private def update(email: String)(change: Room => Room): UIO[Room] =
    rooms.modify: current =>
      val updated = change(current.getOrElse(email, Room()))
      // An empty room is removed rather than kept: nothing should accumulate for accounts that have gone home.
      val next =
        if updated.acquirer.isEmpty && updated.watchers.isEmpty then current - email
        else current.updated(email, updated)
      (updated, next)

  /** Registers the acquirer for an account, displacing any earlier one.
    *
    * Displacing rather than refusing: a phone whose socket died without a close frame would otherwise lock its own
    * account out until the stale entry timed out, and the newest connection is the one the user is looking at.
    */
  def acquirerJoined(email: String, id: String, channel: WebSocketChannel): UIO[Option[WebSocketChannel]] =
    // The displaced channel is taken in the same atomic step that replaces it: reading it afterwards would only
    // ever find the one just installed.
    rooms
      .modify: current =>
        val existing = current.getOrElse(email, Room())
        val displaced = existing.acquirer.collect { case (other, stale) if other != id => stale }
        (displaced, current.updated(email, existing.copy(acquirer = Some(id -> channel))))
      // Watchers are told at once, so a dashboard already open stops saying nothing is counting the moment one is.
      .tap(_ => announce(email))

  def acquirerLeft(email: String, id: String): UIO[Unit] =
    update(email)(current => current.copy(acquirer = current.acquirer.filterNot(_._1 == id))) *> announce(email)

  /** Tells every watcher whether anything is counting, without waiting for the next rep to imply it. */
  private def announce(email: String): UIO[Unit] =
    room(email).flatMap: current =>
      val state = LiveState(current.acquirer.isDefined, current.latest)
      ZIO.foreachParDiscard(current.watchers.values)(send(_, state.toJson).ignore)

  def watcherJoined(email: String, id: String, channel: WebSocketChannel): UIO[LiveState] =
    update(email)(current => current.copy(watchers = current.watchers.updated(id, channel)))
      .map(current => LiveState(current.acquirer.isDefined, current.latest))

  def watcherLeft(email: String, id: String): UIO[Unit] =
    update(email)(current => current.copy(watchers = current.watchers - id)).unit

  /** Sends a reading to every device watching this account, and remembers it for any that connect later. */
  def publish(email: String, reading: LiveReading): UIO[Unit] =
    for
      updated <- update(email)(_.copy(latest = Some(reading)))
      // Failures are ignored per watcher: a socket that has died takes itself out of the room on its own, and one
      // stale entry must not stop the others being told.
      state = LiveState(updated.acquirer.isDefined, Some(reading))
      _ <- ZIO.foreachParDiscard(updated.watchers.values)(send(_, state.toJson).ignore)
    yield ()

  /** Asks this account's acquirer to do something, reporting whether there was one to ask. */
  def command(email: String, instruction: LiveCommand): UIO[Boolean] =
    room(email).flatMap:
      case Room(Some((_, channel)), _, _) => send(channel, instruction.toJson).fold(_ => false, _ => true)
      case _                              => ZIO.succeed(false)

  /** Whether this account currently has a device counting for it. */
  def hasAcquirer(email: String): UIO[Boolean] = room(email).map(_.acquirer.isDefined)

  private[sessions] def send(channel: WebSocketChannel, text: String): Task[Unit] =
    channel.send(Read(WebSocketFrame.text(text)))
