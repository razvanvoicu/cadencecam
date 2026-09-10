package sgrv.fe.acquire

import org.scalajs.dom
import sgrv.api.SignalTrace
import sgrv.fe.HttpService
import zio.json.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.Failure
import scala.util.Success

/** What a capture attempt is doing, so the acquirer can say rather than leave the button looking inert. */
private[fe] enum CaptureState:
  case Idle
  case Sending
  case Captured(reps: Int)
  case Failed(message: String)

/** Sends a minute of the four quadrant signals to the counting session that produced them.
  *
  * The point is to stop guessing. Every threshold in the detector was set by reasoning about signals nobody had
  * recorded, and each guess has been wrong in a different direction — a movement that terminates in its own quadrant, a
  * filter ringing at rep cadence, setup movement that turned out to be accidentally rhythmic. A real recording, of real
  * lighting, can be replayed against a changed detector as often as a question needs asking, which no amount of
  * reasoning about synthetic curls can substitute for.
  */
private[fe] object TraceCapture:

  /** Builds the recording from whatever the buffers currently hold.
    *
    * Deliberately the whole buffer rather than a chosen window: what makes a trace worth keeping is usually something
    * that already happened, and the interesting part is often just before the moment someone reaches for the button.
    */
  private[fe] def of(
      signals: QuadrantSignals,
      reps: Int,
      lock: LockState,
      note: Option[String],
      camera: Option[String] = None,
      controls: Option[String] = None,
      controlsAtSample: Option[Int] = None,
      // The rate belongs to the detector, so a recording is stamped with it from there rather than from the view.
      sampleRateHz: Double = DetectorSettings().sampleRateHz
  ): SignalTrace =
    SignalTrace(
      sampleRateHz = sampleRateHz,
      samples = signals.window(signals.capacity).map((quadrant, values) => quadrant.toString -> values),
      reps = reps,
      lock = describe(lock),
      note = note.map(_.trim).filter(_.nonEmpty),
      camera = camera,
      controls = controls,
      controlsAtSample = controlsAtSample
    )

  /** One line describing what became of the camera's controls, for a trace to carry. */
  private[fe] def describe(outcome: ControlOutcome): String =
    val held = if outcome.held.isEmpty then "none" else outcome.held.mkString("+")
    val skipped = if outcome.skipped.isEmpty then "" else s", left automatic: ${outcome.skipped.mkString("+")}"
    s"${outcome.reason}; held: $held$skipped"

  private[acquire] def describe(lock: LockState): String = lock match
    case LockState.Acquiring(samples, needed)              => s"acquiring $samples/$needed"
    case LockState.Searching                               => "searching"
    case LockState.Locked(channel, partner, periodSeconds) => f"locked $channel+$partner ${periodSeconds}%.2fs"

  /** True when there is enough recorded to be worth sending. */
  private[fe] def worthSending(signals: QuadrantSignals): Boolean = signals.size > 0

  def send(http: HttpService, trace: SignalTrace, onState: CaptureState => Unit): Unit =
    onState(CaptureState.Sending)
    val init = new dom.RequestInit:
      method = dom.HttpMethod.POST
      headers = js.Dictionary("Content-Type" -> "application/json")
      body = trace.toJson
    http
      .send(SignalTrace.Path, init)
      .onComplete:
        case Success(response) if response.ok => onState(CaptureState.Captured(trace.reps))
        case Success(response)                => onState(CaptureState.Failed(s"HTTP ${response.status}"))
        case Failure(error)                   =>
          onState(CaptureState.Failed(Option(error.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse("failed")))
