package sgrv.fe.live

import com.raquo.airstream.state.Var
import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*
import sgrv.api.{PeerRole, PeerSignal, PeerSignals}
import sgrv.fe.HttpService
import sgrv.fe.browser.WebRtcInterop
import sgrv.fe.browser.WebRtcInterop.{Candidate, Channel, Peer}
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

  private[live] val channelLabel = "cadencecam"

  def available: Boolean = WebRtcInterop.available

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
    var connection: Option[Peer] = None
    var channel: Option[Channel] = None
    var waiting = Vector.empty[Candidate]
    var remoteReady = false
    var acted = Set.empty[String]

  private var links = Map.empty[String, Link]
  private var cursor: Option[String] = None
  private var polling: Option[Int] = None
  private var wanted = false

  /** A watcher's own name, fixed for the life of this link; a counter answers to whatever names arrive. */
  private val mine = newPeerId()

  def isOpen: Boolean = links.values.exists(link => link.channel.exists(_.readyState == "open"))

  /** Sends to every open link. A counter says the same thing to each dashboard; a watcher has only the one. */
  def send(text: String): Boolean =
    val open = links.values.flatMap(_.channel).filter(_.readyState == "open")
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
        // No servers configured, deliberately. Both devices are on the same network, so the candidates each browser
        // finds for itself are enough; TURN would relay through a server, which is the thing being escaped.
        val peer = WebRtcInterop.peer()
        link.connection = Some(peer)
        links = links.updated(peerId, link)

        peer.onIceCandidate(candidate => file(PeerSignal(role, "candidate", candidate.json, peerId)))
        peer.onIceConnectionStateChange(() => report(link, "ice"))
        peer.onIceGatheringStateChange(() => report(link, "gathering"))
        peer.onConnectionStateChange: () =>
          report(link, "connection")
          peer.status.connection match
            case "failed" | "closed" | "disconnected" => forget(link)
            case _                                    => ()

        role match
          case PeerRole.Watcher =>
            adopt(link, peer.createDataChannel(channelLabel))
            val _ = peer
              .createOffer()
              .flatMap(offer => peer.setLocalDescription(offer).map(_ => offer))
              .map(offer => file(PeerSignal(role, "offer", offer.json, peerId)))
          case PeerRole.Counter =>
            peer.onDataChannel(channel => adopt(link, channel))
        link

  private def forget(link: Link): Unit =
    shutDown(link)
    links = links - link.peerId
    watchers.set(links.values.count(one => one.channel.exists(_.readyState == "open")))
    if !isOpen then
      phase.set(if wanted then "connecting" else "off")
      onClosed()
      // A counter goes back to watching for whoever appears next; a watcher tries again under the same name.
      if wanted then poll(if role == PeerRole.Counter then pollWhileIdleMillis else pollWhileConnectingMillis)

  private def adopt(link: Link, created: Channel): Unit =
    link.channel = Some(created)
    created.onOpen: () =>
      watchers.set(links.values.count(one => one.channel.exists(_.readyState == "open")))
      phase.set("direct")
      // A counter keeps looking: another dashboard may still want a link of its own.
      if role == PeerRole.Watcher then stopPolling() else poll(pollWhileIdleMillis)
      onOpen()
    created.onMessage(onMessage)
    created.onClose(() => forget(link))

  /** Files what a link is doing, so a pairing that fails can be read afterwards rather than watched live. */
  private def report(link: Link, what: String): Unit =
    link.connection.foreach: peer =>
      val note = WebRtcInterop.diagnosticJson(
        what,
        peer.status,
        link.channel.map(_.readyState).getOrElse("none"),
        links.size,
        link.waiting.size
      )
      file(PeerSignal(role, "state", note, link.peerId))

  private def flush(link: Link, peer: Peer): Unit =
    link.remoteReady = true
    val held = link.waiting
    link.waiting = Vector.empty
    held.foreach(candidate => add(peer, candidate))

  private def add(peer: Peer, candidate: Candidate): Unit =
    try
      val _ = peer
        .addIceCandidate(candidate)
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
      val state = peer.signalingState
      signal.kind match
        // Only while this end is still waiting to be told: one arriving later belongs to an attempt that has moved on.
        case "offer" if role == PeerRole.Counter && state == "stable" =>
          val description = WebRtcInterop.Description.parse(signal.body)
          val _ = peer
            .setRemoteDescription(description)
            .map(_ => flush(link, peer))
            .flatMap(_ => peer.createAnswer())
            .flatMap(answer => peer.setLocalDescription(answer).map(_ => answer))
            .map(answer => file(PeerSignal(role, "answer", answer.json, link.peerId)))
        case "answer" if role == PeerRole.Watcher && state == "have-local-offer" =>
          val description = WebRtcInterop.Description.parse(signal.body)
          val _ = peer
            .setRemoteDescription(description)
            .map(_ => flush(link, peer))
        case "candidate" =>
          val candidate = WebRtcInterop.Candidate.parse(signal.body)
          if link.remoteReady then add(peer, candidate) else link.waiting = link.waiting :+ candidate
        case _ => ()
