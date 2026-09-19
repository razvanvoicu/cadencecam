package sgrv.fe.live

import com.raquo.airstream.state.Var
import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.Thenable.Implicits.*
import sgrv.api.{PeerRole, PeerSignal, PeerSignals}
import sgrv.fe.HttpService
import zio.json.*

/** Direct links between the counting browser and whichever browsers are watching it.
  *
  * The devices are on the same network, so what they have to say to each other need not travel to a server and back. It
  * goes through one while they are being introduced -- an offer, an answer and a handful of candidates, filed under the
  * account's session -- and after that the readings go straight across.
  *
  * That is what removes the problem a server-side relay could not solve: a room held in one server process requires
  * both devices to land in that same process, which nothing about scaling guarantees. Two browsers that have exchanged
  * candidates do not care which instance introduced them, or whether it is still running.
  */
private[fe] object PeerLink:

  /** How often to ask for what the other end has filed while a link is being set up. Stops once one is open. */
  val pollWhileConnectingMillis = 500

  /** How often a counter looks for a watcher that has appeared since. Slower: nothing is waiting on it. */
  val pollWhileIdleMillis = 3000

  /** No servers configured, deliberately. Both devices are on the same network, so the candidates each browser finds
    * for itself are enough; TURN would relay through a server, which is the thing being escaped.
    */
  private[live] val configuration: js.Dynamic = js.Dynamic.literal(iceServers = js.Array())

  private[live] val channelLabel = "cadencecam"

  def available: Boolean =
    !js.isUndefined(js.Dynamic.global.RTCPeerConnection) && js.Dynamic.global.RTCPeerConnection != null

  /** A name for one watching device, so its exchange can be told from another's. */
  private[live] def newPeerId(): String =
    f"${js.Math.floor(js.Math.random() * 0x1000000).toLong}%06x${js.Math.floor(js.Math.random() * 0x1000000).toLong}%06x"

private[fe] final class PeerLink(
    http: HttpService,
    role: PeerRole,
    onMessage: String => Unit,
    onOpen: () => Unit = () => (),
    onClosed: () => Unit = () => ()
):
  import PeerLink.*

  /** What this end is doing, for a screen to show. */
  val phase: Var[String] = Var("off")

  /** How many watching devices are reading directly. Always one or none at a watcher. */
  val watchers: Var[Int] = Var(0)

  /** One connection to one other browser.
    *
    * A counter keeps one of these per watching device: WebRTC connects two ends, so three dashboards are three
    * connections, and an offer, an answer and a set of candidates belong to exactly one of them.
    */
  private final class Link(val peerId: String):
    var connection: Option[js.Dynamic] = None
    var channel: Option[js.Dynamic] = None
    var waiting = Vector.empty[js.Dynamic]
    var remoteReady = false
    var acted = Set.empty[String]

  private var links = Map.empty[String, Link]
  private var cursor: Option[String] = None
  private var polling: Option[Int] = None
  private var wanted = false

  /** A watcher's own name, fixed for the life of this link; a counter answers to whatever names arrive. */
  private val mine = newPeerId()

  def isOpen: Boolean = links.values.exists(link => link.channel.exists(_.readyState.asInstanceOf[String] == "open"))

  /** Sends to every open link. A counter says the same thing to each dashboard; a watcher has only the one. */
  def send(text: String): Boolean =
    val open = links.values.flatMap(_.channel).filter(_.readyState.asInstanceOf[String] == "open")
    open.foldLeft(false): (sent, channel) =>
      try
        channel.send(text)
        true
      catch case _: Throwable => sent

  def connect(): Unit =
    if PeerLink.available && !wanted then
      wanted = true
      phase.set("connecting")
      role match
        case PeerRole.Watcher =>
          // The watcher offers, under its own name; a counter with nobody watching has nothing to offer.
          val _ = start(mine)
          poll(pollWhileConnectingMillis)
        case PeerRole.Counter => poll(pollWhileIdleMillis)

  def close(): Unit =
    wanted = false
    phase.set("off")
    stopPolling()
    links.values.foreach(shutDown)
    links = Map.empty
    watchers.set(0)

  private def shutDown(link: Link): Unit =
    link.channel.foreach(one =>
      try one.close()
      catch case _: Throwable => ()
    )
    link.connection.foreach(one =>
      try one.close()
      catch case _: Throwable => ()
    )

  /** The link to one other browser, built on first mention of its name and never twice.
    *
    * Two connections at one end is the failure this had before names existed: one carried ICE and DTLS while the other
    * answered, and the far side attached to the half that had no channel.
    */
  private def start(peerId: String): Link =
    links.get(peerId) match
      case Some(existing) => existing
      case None           =>
        val link = Link(peerId)
        val peer = js.Dynamic.newInstance(js.Dynamic.global.RTCPeerConnection)(configuration)
        link.connection = Some(peer)
        links = links.updated(peerId, link)

        peer.onicecandidate = { (event: js.Dynamic) =>
          val candidate = event.candidate
          if candidate != null && !js.isUndefined(candidate) then
            file(PeerSignal(role, "candidate", JSON.stringify(candidate.toJSON()), peerId))
        }: js.Function1[js.Dynamic, Unit]
        peer.oniceconnectionstatechange = { (_: js.Dynamic) => report(link, "ice") }: js.Function1[js.Dynamic, Unit]
        peer.onicegatheringstatechange = { (_: js.Dynamic) => report(link, "gathering") }: js.Function1[
          js.Dynamic,
          Unit
        ]
        peer.onconnectionstatechange = { (_: js.Dynamic) =>
          report(link, "connection")
          peer.connectionState.asInstanceOf[String] match
            case "failed" | "closed" | "disconnected" => forget(link)
            case _                                    => ()
        }: js.Function1[js.Dynamic, Unit]

        role match
          case PeerRole.Watcher =>
            adopt(link, peer.createDataChannel(channelLabel))
            val _ = peer
              .createOffer()
              .asInstanceOf[js.Promise[js.Dynamic]]
              .toFuture
              .flatMap(offer =>
                peer.setLocalDescription(offer).asInstanceOf[js.Promise[js.Any]].toFuture.map(_ => offer)
              )
              .map(offer => file(PeerSignal(role, "offer", JSON.stringify(offer), peerId)))
          case PeerRole.Counter =>
            peer.ondatachannel = { (event: js.Dynamic) => adopt(link, event.channel) }: js.Function1[js.Dynamic, Unit]
        link

  private def forget(link: Link): Unit =
    shutDown(link)
    links = links - link.peerId
    watchers.set(links.values.count(one => one.channel.exists(_.readyState.asInstanceOf[String] == "open")))
    if !isOpen then
      phase.set(if wanted then "connecting" else "off")
      onClosed()
      // A counter goes back to watching for whoever appears next; a watcher tries again under the same name.
      if wanted then poll(if role == PeerRole.Counter then pollWhileIdleMillis else pollWhileConnectingMillis)

  private def adopt(link: Link, created: js.Dynamic): Unit =
    link.channel = Some(created)
    created.onopen = { (_: js.Dynamic) =>
      watchers.set(links.values.count(one => one.channel.exists(_.readyState.asInstanceOf[String] == "open")))
      phase.set("direct")
      // A counter keeps looking: another dashboard may still want a link of its own.
      if role == PeerRole.Watcher then stopPolling() else poll(pollWhileIdleMillis)
      onOpen()
    }: js.Function1[js.Dynamic, Unit]
    created.onmessage = { (event: js.Dynamic) => onMessage(event.data.asInstanceOf[String]) }: js.Function1[
      js.Dynamic,
      Unit
    ]
    created.onclose = { (_: js.Dynamic) => forget(link) }: js.Function1[js.Dynamic, Unit]

  /** Files what a link is doing, so a pairing that fails can be read afterwards rather than watched live. */
  private def report(link: Link, what: String): Unit =
    link.connection.foreach: peer =>
      val note = js.Dynamic.literal(
        what = what,
        connection = peer.connectionState,
        ice = peer.iceConnectionState,
        gathering = peer.iceGatheringState,
        signalling = peer.signalingState,
        channel = link.channel.map(_.readyState).getOrElse("none").asInstanceOf[js.Any],
        links = links.size,
        held = link.waiting.size
      )
      file(PeerSignal(role, "state", JSON.stringify(note), link.peerId))

  private def flush(link: Link, peer: js.Dynamic): Unit =
    link.remoteReady = true
    val held = link.waiting
    link.waiting = Vector.empty
    held.foreach(candidate => add(peer, candidate))

  private def add(peer: js.Dynamic, candidate: js.Dynamic): Unit =
    try
      val _ = peer
        .addIceCandidate(candidate)
        .asInstanceOf[js.Promise[js.Any]]
        .toFuture
        .recover { case error => dom.console.warn(s"A candidate was refused: ${error.getMessage}") }
    catch case _: Throwable => dom.console.warn("A candidate could not be added")

  private def file(outgoing: PeerSignal): Unit =
    // Serialised before the request is built: inside the initialiser, `signal` names its own AbortSignal field.
    val payload = outgoing.toJson
    val init = new dom.RequestInit:
      method = dom.HttpMethod.POST
      headers = js.Dictionary("Content-Type" -> "application/json")
      body = payload
    val _ = http.send(PeerSignal.Path, init)

  private def poll(everyMillis: Int): Unit =
    stopPolling()
    if wanted then polling = Some(dom.window.setInterval(() => collect(), everyMillis.toDouble))

  private def stopPolling(): Unit =
    polling.foreach(dom.window.clearInterval)
    polling = None

  private def collect(): Unit =
    val since = cursor.map(at => s"&since=$at").getOrElse("")
    http
      .get(s"${PeerSignal.Path}?role=$role$since")
      .flatMap(response => response.text())
      .map(_.fromJson[PeerSignals])
      .foreach:
        case Right(PeerSignals(signals, next)) =>
          if next.nonEmpty then cursor = Some(next)
          signals.foreach(receive)
        case Left(_) => ()

  private def receive(signal: PeerSignal): Unit =
    // A watcher hears only about its own exchange; a counter about each of them in turn.
    val forMe = role == PeerRole.Counter || signal.peer == mine
    if forMe && signal.kind != "state" then
      val link = if role == PeerRole.Counter then start(signal.peer) else links.get(signal.peer).orNull
      if link != null then
        val fingerprint = s"${signal.kind}:${signal.body.hashCode}"
        if !link.acted.contains(fingerprint) then
          link.acted = link.acted + fingerprint
          apply(link, signal)

  private def apply(link: Link, signal: PeerSignal): Unit =
    link.connection.foreach: peer =>
      val state = peer.signalingState.asInstanceOf[String]
      signal.kind match
        // Only while this end is still waiting to be told: one arriving later belongs to an attempt that has moved on.
        case "offer" if role == PeerRole.Counter && state == "stable" =>
          val description = JSON.parse(signal.body).asInstanceOf[js.Dynamic]
          val _ = peer
            .setRemoteDescription(description)
            .asInstanceOf[js.Promise[js.Any]]
            .toFuture
            .map(_ => flush(link, peer))
            .flatMap(_ => peer.createAnswer().asInstanceOf[js.Promise[js.Dynamic]].toFuture)
            .flatMap(answer =>
              peer.setLocalDescription(answer).asInstanceOf[js.Promise[js.Any]].toFuture.map(_ => answer)
            )
            .map(answer => file(PeerSignal(role, "answer", JSON.stringify(answer), link.peerId)))
        case "answer" if role == PeerRole.Watcher && state == "have-local-offer" =>
          val description = JSON.parse(signal.body).asInstanceOf[js.Dynamic]
          val _ = peer
            .setRemoteDescription(description)
            .asInstanceOf[js.Promise[js.Any]]
            .toFuture
            .map(_ => flush(link, peer))
        case "candidate" =>
          val candidate = JSON.parse(signal.body).asInstanceOf[js.Dynamic]
          if link.remoteReady then add(peer, candidate) else link.waiting = link.waiting :+ candidate
        case _ => ()
