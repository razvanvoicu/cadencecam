package sgrv.fe.live

import org.scalajs.dom
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.Thenable.Implicits.*
import sgrv.api.{PeerRole, PeerSignal, PeerSignals}
import sgrv.fe.HttpService
import zio.json.*

/** A direct link between the two browsers, with the server used only to introduce them.
  *
  * The counting device and the watching one are on the same network, so what they have to say to each other need not
  * travel to a server and back at all. It goes through one while the link is being set up -- an offer, an answer and a
  * handful of candidates, filed under the account's session -- and after that the readings go straight across.
  *
  * That removes the problem the relay could not solve: a room held in one server process requires both devices to land
  * in that same process, which nothing about scaling guarantees. Two browsers that have exchanged candidates do not
  * care which instance introduced them, or whether it is still running.
  */
private[fe] object PeerLink:

  /** How often to ask for what the other end has filed, while a link is being set up.
    *
    * Only while connecting. Once the channel opens the polling stops, so the cost is a handful of requests per pairing
    * rather than anything standing.
    */
  val pollWhileConnectingMillis = 500

  /** How often the counter looks for a watcher that has appeared since, with no link up.
    *
    * Slower, because nothing is waiting on it: a dashboard opening has to be noticed within a few seconds, and until
    * one does the counter has nobody to talk to.
    */
  val pollWhileIdleMillis = 3000

  /** No servers configured, deliberately.
    *
    * Both devices are on the same network, so the candidates each browser finds for itself are enough. A STUN server
    * only helps across networks, and TURN means relaying through a server -- which is the thing being escaped.
    */
  private[live] val configuration: js.Dynamic = js.Dynamic.literal(iceServers = js.Array())

  /** The name of the one channel a link carries. Both ends must agree on it. */
  private[live] val channelLabel = "cadencecam"

  /** Whether a browser can do this at all. Safari on iOS can; a browser without it falls back to the relay. */
  def available: Boolean =
    !js.isUndefined(js.Dynamic.global.RTCPeerConnection) && js.Dynamic.global.RTCPeerConnection != null

private[fe] final class PeerLink(
    http: HttpService,
    role: PeerRole,
    onMessage: String => Unit,
    onOpen: () => Unit = () => (),
    onClosed: () => Unit = () => ()
):
  import PeerLink.*

  private var connection: Option[js.Dynamic] = None
  private var channel: Option[js.Dynamic] = None
  private var cursor: Option[String] = None
  private var polling: Option[Int] = None
  private var wanted = false

  /** What has already been acted on, so a signal read twice is not acted on twice.
    *
    * The window a cursor falls back to is a couple of minutes wide, so an offer can be handed over again long after it
    * was answered. Answering it a second time renegotiates a link that was working, or builds one against a peer that
    * has moved on.
    */
  private var acted = Set.empty[String]

  /** Candidates that arrived before there was a description to attach them to.
    *
    * `setRemoteDescription` resolves a promise, and the candidates for a link usually arrive in the same batch as the
    * offer or answer they belong to. Adding one before that promise settles is rejected -- there is no remote
    * description yet -- so every candidate was being dropped, and a handshake that read perfectly produced a link that
    * never came up, because neither end had an address to try.
    */
  private var waitingCandidates = Vector.empty[js.Dynamic]

  private var remoteReady = false

  def isOpen: Boolean = channel.exists(_.readyState.asInstanceOf[String] == "open")

  /** Sends over the link, or reports that there is no link to send over. */
  def send(text: String): Boolean =
    channel match
      case Some(open) if isOpen =>
        try
          open.send(text)
          true
        catch case _: Throwable => false
      case _ => false

  def connect(): Unit =
    if !wanted && PeerLink.available then
      wanted = true
      start()

  def close(): Unit =
    wanted = false
    stopPolling()
    channel.foreach(one =>
      try one.close()
      catch case _: Throwable => ()
    )
    connection.foreach(one =>
      try one.close()
      catch case _: Throwable => ()
    )
    channel = None
    connection = None

  private def start(): Unit =
    val peer = js.Dynamic.newInstance(js.Dynamic.global.RTCPeerConnection)(configuration)
    connection = Some(peer)
    peer.onicecandidate = { (event: js.Dynamic) =>
      val candidate = event.candidate
      if candidate != null && !js.isUndefined(candidate) then
        val _ = file(PeerSignal(role, "candidate", JSON.stringify(candidate.toJSON())))
    }: js.Function1[js.Dynamic, Unit]
    // Filed rather than only logged. These devices are a phone and a laptop on a bench; reading a console on either
    // is awkward, and what the link is doing is the one thing that cannot be worked out from the outside.
    peer.oniceconnectionstatechange = { (_: js.Dynamic) =>
      report(peer, "ice")
    }: js.Function1[js.Dynamic, Unit]
    peer.onicegatheringstatechange = { (_: js.Dynamic) =>
      report(peer, "gathering")
    }: js.Function1[js.Dynamic, Unit]
    peer.onconnectionstatechange = { (_: js.Dynamic) =>
      report(peer, "connection")
      peer.connectionState.asInstanceOf[String] match
        case "failed" | "closed" | "disconnected" =>
          onClosed()
          // Left to the caller to decide whether to try again: a watcher will, a counter waits to be found.
          ()
        case _ => ()
    }: js.Function1[js.Dynamic, Unit]

    role match
      case PeerRole.Watcher =>
        // The watcher creates the channel and offers; a counter with nobody watching has nothing to offer.
        val created = peer.createDataChannel(channelLabel)
        adopt(created)
        val _ = peer
          .createOffer()
          .asInstanceOf[js.Promise[js.Dynamic]]
          .toFuture
          .flatMap: offer =>
            peer.setLocalDescription(offer).asInstanceOf[js.Promise[js.Any]].toFuture.map(_ => offer)
          .map(offer => file(PeerSignal(role, "offer", JSON.stringify(offer))))
      case PeerRole.Counter =>
        peer.ondatachannel = { (event: js.Dynamic) => adopt(event.channel) }: js.Function1[js.Dynamic, Unit]

    poll(if role == PeerRole.Watcher then pollWhileConnectingMillis else pollWhileIdleMillis)

  private def adopt(created: js.Dynamic): Unit =
    channel = Some(created)
    created.onopen = { (_: js.Dynamic) =>
      // Nothing more to introduce: the two ends are talking, so the server is left out of it from here.
      stopPolling()
      onOpen()
    }: js.Function1[js.Dynamic, Unit]
    created.onmessage = { (event: js.Dynamic) =>
      onMessage(event.data.asInstanceOf[String])
    }: js.Function1[js.Dynamic, Unit]
    created.onclose = { (_: js.Dynamic) =>
      onClosed()
      if wanted then poll(pollWhileIdleMillis)
    }: js.Function1[js.Dynamic, Unit]

  /** Files what the link is doing, so a pairing that fails can be read afterwards rather than watched live. */
  private def report(peer: js.Dynamic, what: String): Unit =
    val note = js.Dynamic.literal(
      what = what,
      connection = peer.connectionState,
      ice = peer.iceConnectionState,
      gathering = peer.iceGatheringState,
      signalling = peer.signalingState,
      channel = channel.map(_.readyState).getOrElse("none").asInstanceOf[js.Any],
      offered = acted.size,
      held = waitingCandidates.size
    )
    file(PeerSignal(role, "state", JSON.stringify(note)))

  /** Hands over everything that was waiting for a description, and lets later ones through directly. */
  private def flush(peer: js.Dynamic): Unit =
    remoteReady = true
    val held = waitingCandidates
    waitingCandidates = Vector.empty
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
    if wanted then
      polling = Some(
        dom.window.setInterval(
          () => collect(),
          everyMillis.toDouble
        )
      )

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
    val fingerprint = s"${signal.kind}:${signal.body.hashCode}"
    if acted.contains(fingerprint) then ()
    else
      acted = acted + fingerprint
      apply(signal)

  private def apply(signal: PeerSignal): Unit =
    connection.foreach: peer =>
      val state = peer.signalingState.asInstanceOf[String]
      signal.kind match
        case "state" => ()
        // Only while this end is still waiting to be told: an offer arriving after the description is set belongs to
        // an attempt that has moved on.
        case "offer" if role == PeerRole.Counter && state == "stable" =>
          val description = JSON.parse(signal.body).asInstanceOf[js.Dynamic]
          val _ = peer
            .setRemoteDescription(description)
            .asInstanceOf[js.Promise[js.Any]]
            .toFuture
            .map(_ => flush(peer))
            .flatMap(_ => peer.createAnswer().asInstanceOf[js.Promise[js.Dynamic]].toFuture)
            .flatMap: answer =>
              peer.setLocalDescription(answer).asInstanceOf[js.Promise[js.Any]].toFuture.map(_ => answer)
            .map(answer => file(PeerSignal(role, "answer", JSON.stringify(answer))))
        case "answer" if role == PeerRole.Watcher && state == "have-local-offer" =>
          val description = JSON.parse(signal.body).asInstanceOf[js.Dynamic]
          val _ = peer
            .setRemoteDescription(description)
            .asInstanceOf[js.Promise[js.Any]]
            .toFuture
            .map(_ => flush(peer))
        case "candidate" =>
          val candidate = JSON.parse(signal.body).asInstanceOf[js.Dynamic]
          if remoteReady then add(peer, candidate) else waitingCandidates = waitingCandidates :+ candidate
        case _ => ()
