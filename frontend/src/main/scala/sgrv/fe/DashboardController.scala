package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.{LiveCommand, LiveReading, LiveState, PeerRole}
import sgrv.fe.acquire.StatusLine
import sgrv.fe.live.PeerLink
import zio.json.*

/** Owns the dashboard's direct connection to the counting device and the readings received over it. */
private[fe] final class DashboardController(http: HttpService, initialExercise: Option[String]):
  private val live = Var(Option.empty[LiveState])
  private val connected = Var(false)
  private val selected = Var(initialExercise)
  private var exerciseSynchronized = false

  val reading: Signal[Option[LiveReading]] = live.signal.map(_.flatMap(_.reading))
  val exerciseId: Signal[Option[String]] = selected.signal
  val reps: Signal[Int] = reading.map(_.fold(0)(_.reps))
  val elapsedSeconds: Signal[Double] = reading.map(_.fold(0.0)(_.elapsedSeconds))
  val workoutActive: Signal[Boolean] = reading.map(_.exists(_.active))

  /** The last few reps, as they were when each was counted. Kept here rather than sent, because a pace is a property of
    * the reps a watcher has seen and the counter has no reason to hold a second window of its own.
    */
  val window: Var[Vector[Effort.RepMark]] = Var(Vector.empty)

  /** What the link and the counter behind it are doing, in the counter's own words where it has any.
    *
    * Until a counting device answers there is no link, and that is also how an account with nothing counting looks from
    * here: both say they are connecting. Once readings arrive, the counter's status line is repeated word for word, so
    * the two screens cannot describe the same moment differently.
    */
  val status: Signal[String] = live.signal
    .combineWith(connected.signal)
    .map:
      case (_, false)                                 => "Connecting…"
      case (Some(state), _) if state.reading.nonEmpty =>
        val latest = state.reading.get
        if latest.active then latest.status else "Ready"
      case _ => StatusLine.Waiting

  private def readUpdate(text: String): Unit =
    text.fromJson[LiveState] match
      case Right(state) =>
        live.set(Some(state))
        state.exerciseSelection match
          case Some(selection) =>
            selected.set(selection.exerciseId)
            exerciseSynchronized = true
          case None if !exerciseSynchronized =>
            // The first dashboard supplies the initial choice. A later dashboard receives the counter's cached choice
            // instead, so merely connecting cannot overwrite an exercise already shared by the room.
            exerciseSynchronized = true
            announceExercise()
          case None => ()
        state.reading.foreach(mark)
      case Left(details) => dom.console.warn(s"Ignoring an unreadable update: $details")

  private def mark(latest: LiveReading): Unit =
    window.update: marks =>
      Effort.noting(
        marks,
        Effort.RepMark(latest.reps, latest.lastRepSeconds, latest.cadenceSum)
      )

  /** The direct link to the counting device, which the readings travel over.
    *
    * The watcher offers, because it is the one that wants the data. There is no other path: the server introduces the
    * two and then has nothing further to do with them.
    */
  private val peer: PeerLink = PeerLink(
    http,
    PeerRole.Watcher,
    readUpdate,
    onOpen = () => connected.set(true),
    onClosed = () => {
      connected.set(false)
      exerciseSynchronized = false
    }
  )

  def connect(): Unit = peer.connect()

  def close(): Unit = peer.close()

  def currentReading: Option[LiveReading] = live.now().flatMap(_.reading)

  def currentExerciseId: Option[String] = selected.now()

  /** Changes this dashboard immediately, then asks the counter to relay only the opaque id to the others. */
  def chooseExercise(exerciseId: Option[String]): Unit =
    selected.set(exerciseId)
    if exerciseSynchronized then announceExercise()

  private def announceExercise(): Unit =
    if connected.now() then
      val command: LiveCommand = LiveCommand.SelectExercise(selected.now())
      val _ = peer.send(command.toJson)

  def ask(instruction: LiveCommand): Unit =
    val _ = peer.send(instruction.toJson)
