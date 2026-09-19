package sgrv.fe.browser

import org.scalajs.dom
import scala.annotation.nowarn
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/** A narrow facade over the modern WebRTC surface used by the app.
  *
  * The scalajs-dom version in this project predates `connectionState`, candidate `toJSON`, and a few current event
  * shapes. Keeping the native declarations and nullable event values here lets the link state machine deal only in
  * Scala values.
  */
private[fe] object WebRtcInterop:

  final class Description private[WebRtcInterop] (private[WebRtcInterop] val raw: NativeDescription):
    def json: String = js.JSON.stringify(raw)

  object Description:
    def parse(json: String): Description = Description(js.JSON.parse(json).asInstanceOf[NativeDescription])

  final class Candidate private[WebRtcInterop] (private[WebRtcInterop] val raw: NativeCandidate):
    def json: String = js.JSON.stringify(raw.toJSON())

  object Candidate:
    def parse(json: String): Candidate = Candidate(js.JSON.parse(json).asInstanceOf[NativeCandidate])

  final case class Status(connection: String, ice: String, gathering: String, signalling: String)

  final class Channel private[WebRtcInterop] (private val raw: NativeDataChannel):
    def readyState: String = raw.readyState
    def send(text: String): Unit = raw.send(text)
    def close(): Unit = raw.close()

    def onOpen(listener: () => Unit): Unit = raw.onopen = (_: dom.Event) => listener()
    def onMessage(listener: String => Unit): Unit =
      raw.onmessage = (event: NativeMessageEvent) => listener(event.data)
    def onClose(listener: () => Unit): Unit = raw.onclose = (_: dom.Event) => listener()

  final class Peer private[WebRtcInterop] (private val raw: NativePeerConnection):
    def status: Status = Status(raw.connectionState, raw.iceConnectionState, raw.iceGatheringState, raw.signalingState)
    def signalingState: String = raw.signalingState

    def onIceCandidate(listener: Candidate => Unit): Unit =
      raw.onicecandidate = (event: NativeIceCandidateEvent) =>
        val candidate = event.candidate
        if candidate != null && !js.isUndefined(candidate) then listener(Candidate(candidate))

    def onIceConnectionStateChange(listener: () => Unit): Unit =
      raw.oniceconnectionstatechange = (_: dom.Event) => listener()

    def onIceGatheringStateChange(listener: () => Unit): Unit =
      raw.onicegatheringstatechange = (_: dom.Event) => listener()

    def onConnectionStateChange(listener: () => Unit): Unit =
      raw.onconnectionstatechange = (_: dom.Event) => listener()

    def onDataChannel(listener: Channel => Unit): Unit =
      raw.ondatachannel = (event: NativeDataChannelEvent) => listener(Channel(event.channel))

    def createDataChannel(label: String): Channel = Channel(raw.createDataChannel(label))
    def createOffer(): Future[Description] = raw.createOffer().toFuture.map(Description.apply)
    def createAnswer(): Future[Description] = raw.createAnswer().toFuture.map(Description.apply)
    def setLocalDescription(description: Description): Future[Unit] =
      raw.setLocalDescription(description.raw).toFuture
    def setRemoteDescription(description: Description): Future[Unit] =
      raw.setRemoteDescription(description.raw).toFuture
    def addIceCandidate(candidate: Candidate): Future[Unit] = raw.addIceCandidate(candidate.raw).toFuture
    def close(): Unit = raw.close()

  def available: Boolean =
    !js.isUndefined(js.Dynamic.global.RTCPeerConnection) && js.Dynamic.global.RTCPeerConnection != null

  def peer(): Peer =
    val configuration = js.Dynamic.literal(iceServers = js.Array()).asInstanceOf[NativeConfiguration]
    Peer(new NativePeerConnection(configuration))

  /** Serialises connection diagnostics without exposing a JavaScript dictionary to the link state machine. */
  def diagnosticJson(
      what: String,
      status: Status,
      channel: String,
      links: Int,
      held: Int
  ): String =
    js.JSON.stringify(
      js.Dynamic.literal(
        what = what,
        connection = status.connection,
        ice = status.ice,
        gathering = status.gathering,
        signalling = status.signalling,
        channel = channel,
        links = links,
        held = held
      )
    )

  @js.native
  private trait NativeConfiguration extends js.Object

  @js.native
  private trait NativeDescription extends js.Object

  @js.native
  private trait NativeCandidate extends js.Object:
    def toJSON(): js.Object = js.native

  @js.native
  private trait NativeIceCandidateEvent extends dom.Event:
    val candidate: NativeCandidate = js.native

  @js.native
  private trait NativeDataChannelEvent extends dom.Event:
    val channel: NativeDataChannel = js.native

  @js.native
  private trait NativeMessageEvent extends dom.Event:
    val data: String = js.native

  @js.native
  private trait NativeDataChannel extends js.Object:
    val readyState: String = js.native
    var onopen: js.Function1[dom.Event, Any] = js.native
    var onmessage: js.Function1[NativeMessageEvent, Any] = js.native
    var onclose: js.Function1[dom.Event, Any] = js.native
    def send(text: String): Unit = js.native
    def close(): Unit = js.native

  @js.native
  @JSGlobal("RTCPeerConnection")
  @nowarn("msg=unused explicit parameter")
  private class NativePeerConnection(_configuration: NativeConfiguration) extends js.Object:
    val connectionState: String = js.native
    val iceConnectionState: String = js.native
    val iceGatheringState: String = js.native
    val signalingState: String = js.native
    var onicecandidate: js.Function1[NativeIceCandidateEvent, Any] = js.native
    var oniceconnectionstatechange: js.Function1[dom.Event, Any] = js.native
    var onicegatheringstatechange: js.Function1[dom.Event, Any] = js.native
    var onconnectionstatechange: js.Function1[dom.Event, Any] = js.native
    var ondatachannel: js.Function1[NativeDataChannelEvent, Any] = js.native
    def createDataChannel(label: String): NativeDataChannel = js.native
    def createOffer(): js.Promise[NativeDescription] = js.native
    def createAnswer(): js.Promise[NativeDescription] = js.native
    def setLocalDescription(description: NativeDescription): js.Promise[Unit] = js.native
    def setRemoteDescription(description: NativeDescription): js.Promise[Unit] = js.native
    def addIceCandidate(candidate: NativeCandidate): js.Promise[Unit] = js.native
    def close(): Unit = js.native
