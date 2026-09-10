package sgrv.be.sessions

import java.security.SecureRandom
import sgrv.api.{Live, LiveCommand, LiveReading}
import sgrv.be.BackendCapabilities
import sgrv.be.auth.SessionStore
import sgrv.be.core.{AccessPolicy, BackendPlugin, CapabilitySet, RequestContext}
import zio.*
import zio.http.*
import zio.http.ChannelEvent.{ExceptionCaught, Read, Registered, Unregistered, UserEventTriggered}
import zio.http.ChannelEvent.UserEvent.HandshakeComplete
import zio.json.*

/** Carries readings from an account's acquirer to its dashboards, and commands back the other way.
  *
  * Both ends authenticate as themselves and are paired by account, so nothing in either message says whose count it
  * is: a device can report or command only for the user holding it. The dashboard cannot address an acquirer at all,
  * only ask its own account's.
  */
object LiveRelay extends BackendPlugin:
  type Requires = SessionStore

  override val id = "live-relay"
  override val requirements: CapabilitySet[Requires] = CapabilitySet.one(BackendCapabilities.sessionStore)
  override val accessPolicy: AccessPolicy[Requires] = AccessPolicy.Authenticated
  override val routes: Routes[Requires & RequestContext, Nothing] = Routes(
    Method.GET / "ws" / "acquirer" -> handler(asUser(acquirer)),
    Method.GET / "ws" / "dashboard" -> handler(asUser(dashboard))
  )

  /** Resolves who is connecting before opening the socket, so the account is fixed for the connection's lifetime and
    * never has to be taken from a message.
    */
  private def asUser(open: String => ZIO[Any, Nothing, Response]): ZIO[RequestContext, Nothing, Response] =
    ZIO.serviceWithZIO[RequestContext]:
      case RequestContext.Authenticated(_, user) => open(user.email)
      case _                                     => ZIO.succeed(Response.status(Status.Unauthorized))

  /** The event that means a server-side socket is ready to use.
    *
    * Not `Registered`, which is Netty's channel-registration callback and fires when a connection is first accepted.
    * On the server the WebSocket handler joins the pipeline only after the upgrade, by which time registration is
    * long past, so `Registered` never arrives and anything waiting for it waits forever. The handshake completing is
    * the signal that actually marks a usable socket at this end.
    */
  private object Ready:
    def unapply(event: WebSocketChannelEvent): Boolean = event match
      case UserEventTriggered(HandshakeComplete) => true
      // Kept as well: it is the right signal for a socket opened as a client, and harmless here.
      case Registered => true
      case _          => false

  private def acquirer(email: String): ZIO[Any, Nothing, Response] =
    val connection = connectionId()
    Handler
      .webSocket: channel =>
        channel.receiveAll:
          case Ready() =>
            LiveSessions.acquirerJoined(email, connection, channel).flatMap:
              case Some(displaced) =>
                // The account already had one. The newest connection is the device the user is looking at, so the
                // older is closed rather than left to report into a room it no longer owns.
                ZIO.logInfo(s"A second acquirer joined for $email; closing the first") *> displaced.shutdown
              case None => ZIO.logInfo(s"Acquirer joined for $email")
          case Read(WebSocketFrame.Text(text)) =>
            text.fromJson[LiveReading] match
              case Right(reading) => LiveSessions.publish(email, reading)
              case Left(details)  => ZIO.logWarning(s"Ignoring an unreadable reading from $email: $details")
          case Unregistered          => LiveSessions.acquirerLeft(email, connection)
          case ExceptionCaught(_)    => LiveSessions.acquirerLeft(email, connection)
          case _                     => ZIO.unit
      .toResponse

  private def dashboard(email: String): ZIO[Any, Nothing, Response] =
    val connection = connectionId()
    Handler
      .webSocket: channel =>
        channel.receiveAll:
          case Ready() =>
            // Whether anything is counting, and whatever it last said: a dashboard opened mid-set shows the count
            // at once, and one opened before the other device says so rather than looking broken.
            LiveSessions
              .watcherJoined(email, connection, channel)
              .flatMap(state => channel.send(Read(WebSocketFrame.text(state.toJson))).ignore)
          case Read(WebSocketFrame.Text(text)) =>
            text.fromJson[LiveCommand] match
              case Right(instruction) =>
                LiveSessions.command(email, instruction).flatMap: delivered =>
                  ZIO.logInfo(s"$instruction from a dashboard for $email, delivered: $delivered")
              case Left(details) => ZIO.logWarning(s"Ignoring an unreadable command from $email: $details")
          case Unregistered       => LiveSessions.watcherLeft(email, connection)
          case ExceptionCaught(_) => LiveSessions.watcherLeft(email, connection)
          case _                  => ZIO.unit
      .toResponse

  /** Distinguishes one connection from another for the same account, so a socket that goes away removes itself
    * rather than whichever one happens to be registered.
    */
  private[sessions] def connectionId(): String =
    val bytes = new Array[Byte](8)
    SecureRandom().nextBytes(bytes)
    bytes.map(byte => f"${byte & 0xff}%02x").mkString

  private[sessions] val paths = (Live.AcquirerPath, Live.DashboardPath)
