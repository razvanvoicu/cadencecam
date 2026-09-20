package sgrv.fe

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.{
  AccountSettings,
  LiveCommand,
  LiveReading,
  LiveState,
  PeerRole,
  RepProgress,
  WorkoutAction,
  WorkoutSnapshot
}
import sgrv.fe.acquire.{
  Adjustable,
  Camera,
  CameraAdjust,
  CameraDevice,
  CameraState,
  CaptureState,
  FrameSampler,
  LockState,
  Quadrant,
  QuadrantSignals,
  RepCountStore,
  RepCounter,
  RepProgressReporter,
  RestPhase,
  ResumedRun,
  Sample,
  SignalGraph,
  StatusLine,
  TraceCapture
}
import sgrv.fe.live.PeerLink
import sgrv.fe.browser.CameraInterop
import zio.json.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.util.Failure
import scala.util.Success

/** The camera, sampler and counter lifecycle for the device doing the counting.
  *
  * These browser resources live outside persisted [[FrontendState]] and are torn down when this view unmounts —
  * otherwise the camera would stay held after leaving the screen.
  */
private[fe] object CounterScreen:
  def apply(
      api: ApiClient,
      repCountStore: RepCountStore,
      settingsPanel: SettingsPanel,
      show: Screen => Unit,
      openAbout: () => Unit,
      logout: () => Unit
  ): Element =
    val cameraState = Var[CameraState](CameraState.Idle)
    // Reps counted before the current detector run, which the detector itself knows nothing about: those carried over
    // from a previous page load, and those counted before a camera switch. Shown at once, so a reload mid-set does not
    // look as though it lost the count while the detector spends its first seconds finding the cadence.
    var baseline = repCountStore.restore(js.Date.now())
    val repCount = Var(baseline.count)
    val lock = Var[LockState](LockState.Acquiring(0, 0))
    // How far the movement stands above the background, kept whether or not a cadence has been found: a movement too
    // faint to count looks from the outside exactly like no movement at all.
    val signalMargin = Var(Option.empty[Double])
    // Derived from the camera itself rather than chosen: one on the same side as the screen is shown mirrored, one
    // facing away is not. Not persisted, since it belongs to the hardware rather than to the user.
    val mirrored = Var(false)
    val devices = Var(Seq.empty[CameraDevice])
    val currentDevice = Var(Option.empty[String])
    val menuOpen = Var(false)
    val workoutActive = Var(false)
    val workoutBusy = Var(false)
    // Automatic-to-fixed camera changes are not signal. The sampler keeps running so the new picture can be checked,
    // but these frames are excluded from the detector and the last good preview is held over the live video.
    val controlsTransitioning = Var(false)
    val freezeVisible = Var(false)
    // Where each channel's peak falls within a rep, once the buffer has shown the hand at rest. Kept per quadrant and
    // retained: the opening stillness scrolls out of the minute, but what it established stays true, and a leader
    // switch should show that leader's own offset rather than the previous one's.
    val restOffsets = Var(Map.empty[Quadrant, Double])
    val capture = Var[CaptureState](CaptureState.Idle)
    // Composed once and used twice: shown here, and sent verbatim to whatever is watching. Re-deriving the wording at
    // the other end would let the two screens drift apart on the first change to either.
    val statusText = lock.signal
      .combineWith(restOffsets.signal)
      .map: (state, offsets) =>
        state match
          case LockState.Locked(channel, _, _) =>
            // How far into a rep this channel's tick lands, when the buffer has been able to say. Shown while it is only
            // a diagnostic: it is the number that decides whether a tick agrees with the exerciser.
            val phase = offsets.get(channel).fold("")(offset => f" · φ$offset%.2f")
            StatusLine.of(state) + phase
          case other => StatusLine.of(other)
    val shownStatus = statusText
      .combineWith(workoutActive.signal, workoutBusy.signal)
      .map:
        case (_, true, true)         => "Stopping…"
        case (_, false, true)        => "Starting…"
        case (_, false, false)       => "Ready"
        case (detector, true, false) => detector
    var latestStatus = StatusLine.of(LockState.Acquiring(0, 0))
    val counter = RepCounter()
    val reporter = RepProgressReporter(api.http)
    var totalSamples = 0
    val signals = QuadrantSignals()
    val tick = Var(0)
    // Opening a camera is asynchronous, and this view can be gone before it finishes. Without this the stream arrives
    // to a dead view, starts a sampler nobody stops, and that sampler goes on writing the stored count from behind
    // whatever the user is actually looking at.
    var live = true
    // Captured when the camera opens, so a trace can say what this device's camera reported about itself.
    var cameraReport = Option.empty[String]
    var controlNote = Option.empty[String]
    var controlsAtSample = Option.empty[Int]
    var stream: Option[dom.MediaStream] = None
    var sampler: Option[FrameSampler] = None
    // What this camera lets a person move by hand. Empty until a camera is open, and on any camera that reports its
    // settings as modes rather than ranges -- Safari offers no exposure at all, so there the panel simply is not there
    // rather than being there and inert.
    val adjustables = Var(Seq.empty[Adjustable])
    val adjustOpen = Var(false)
    val roleRequests = RequestScope()
    val workoutRequests = RequestScope()
    // Frozen at Start so editing settings during a workout cannot rewrite what that workout means in history.
    var workoutSettings: AccountSettings = settingsPanel.settings.now()

    val video = videoTag(cls := "camera-video")
    val frozen = canvasTag(
      cls := "camera-freeze",
      cls("visible") <-- freezeVisible.signal,
      aria.hidden := true
    )
    // The overlay's lines sit at 50% of this box, so the box must be exactly the frame: its aspect ratio is set from the
    // stream once known, keeping the drawn quadrants aligned with the sampled ones.
    val frame = div(
      cls := "camera-frame",
      cls("mirrored") <-- mirrored.signal,
      video,
      frozen,
      div(cls := "quadrant-lines")
    )

    // Set on the element rather than as Laminar attributes: playsinline in particular is what stops iOS taking the
    // preview fullscreen, and it has to be a real attribute on the node before play() is called.
    def prepare(element: dom.HTMLVideoElement): Unit =
      CameraInterop.preparePreview(element)

    var lastPublished = Option.empty[LiveReading]

    /** Sends a reading only when it says something new.
      *
      * Samples arrive ten times a second and almost all of them repeat the last: the count is unchanged between reps,
      * and the pace is only worth reporting to the nearest whole number a screen will show.
      */
    def publish(): Unit =
      val reading =
        LiveReading(
          repCount.now(),
          math.round(counter.repsPerMinute).toDouble,
          if workoutActive.now() then latestStatus else "Ready",
          Device.describe(),
          counting = workoutActive.now() && !workoutBusy.now() && lock.now().isInstanceOf[LockState.Locked],
          active = workoutActive.now(),
          // Both carry whatever a previous page load left behind, for the same reason the count does: a watching device
          // must not see a set restart because the phone showing it reloaded.
          // Whole seconds. The clock runs on while a set rests, so an unrounded figure would differ on every one of the
          // ten samples a second and publish ten updates a second to say so.
          elapsedSeconds = math.round(baseline.elapsedSeconds + counter.elapsedSeconds).toDouble,
          cadenceSum = baseline.cadenceSum + counter.reading.cadenceSum,
          // To a tenth, which is the sample clock's own resolution: this is what a cadence is measured over, and it
          // changes only when a rep is counted, so it costs nothing to send exactly.
          lastRepSeconds = math.round((baseline.elapsedSeconds + counter.lastRepSeconds) * 10.0) / 10.0
        )
      if !lastPublished.contains(reading) then
        // To whoever is watching, over their own link. Nothing is published when nobody is: a count with no audience is
        // the ordinary case, and there is no server left to tell.
        if peer.send(LiveState(acquiring = true, reading = Some(reading)).toJson) then lastPublished = Some(reading)

    def countSample(sample: Sample): Unit =
      // The detector's own window, not the whole buffer. The buffer is now six minutes so a trace can carry a whole
      // session, and re-filtering all of it on every sample would be five times the work at ten hertz.
      val window = signals.window(QuadrantSignals.DetectionWindow)
      val reading = counter.update(window, totalSamples)
      reading.lock match
        case LockState.Locked(channel, _, _) =>
          RestPhase.offsetOf(window, channel).foreach(offset => restOffsets.update(_ + (channel -> offset)))
        case _ => ()
      val total = baseline.count + reading.count
      // Written on change rather than on every sample: ten writes a second would record nothing new between reps. The
      // sample's own timestamp dates the reading, so the age a later load measures is the age of the last rep rather
      // than of the last frame.
      if total != repCount.now() then
        repCountStore.save(
          total,
          sample.atMillis,
          baseline.cadenceSum + reading.cadenceSum,
          baseline.elapsedSeconds + reading.elapsedSeconds
        )
      repCount.set(total)
      lock.set(reading.lock)
      signalMargin.set(reading.margin)
      publish()

    def onSample(sample: Sample): Unit =
      signals.record(sample)
      totalSamples += 1
      // The camera and traces remain live while stopped, but the detector is frozen. Otherwise movement between
      // workouts would silently become the opening reps of the next one before Start had been pressed.
      // A control transition is also frozen: its global brightness step is caused by the camera rather than exercise,
      // and letting it into the detector would manufacture the same common signal the fixed controls are preventing.
      if workoutActive.now() && !workoutBusy.now() && !controlsTransitioning.now() then countSample(sample)
      tick.update(_ + 1)

    /** Covers a camera-control transition with the last good frame and starts the detector on clean samples after it.
      *
      * The live samples continue underneath because Camera uses them to verify the manual fallback's brightness. They
      * are discarded before detection resumes, so neither a dark fallback frame nor a one-shot metering adjustment can
      * become a repetition. Any count already earned is first absorbed into the baseline, just as it is for a camera
      * switch, so fixing the camera mid-set cannot take reps away.
      */
    def controlTransition(active: Boolean): Unit =
      if live && active then
        controlsTransitioning.set(true)
        val source = video.ref
        val canvas = frozen.ref
        val box = frame.ref.getBoundingClientRect()
        val ratio = dom.window.devicePixelRatio
        val width = math.max(1, (box.width * ratio).round.toInt)
        val height = math.max(1, (box.height * ratio).round.toInt)
        val captured =
          if source.readyState.asInstanceOf[Int] >= 2 && source.videoWidth > 0 && source.videoHeight > 0
          then
            try
              canvas.width = width
              canvas.height = height
              val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
              context.drawImage(source, 0, 0, width, height)
              true
            catch case _: Throwable => false
          else false
        freezeVisible.set(captured)
        baseline = baseline.plus(counter.reading)
        counter.reset()
        repCount.set(baseline.count)
        signals.clear()
        totalSamples = 0
        restOffsets.set(Map.empty)
        lock.set(LockState.Acquiring(0, 0))
        signalMargin.set(None)
      else if live && !active then
        // Throw away the transition frames which deliberately stayed available to the brightness validator.
        signals.clear()
        totalSamples = 0
        restOffsets.set(Map.empty)
        lock.set(LockState.Acquiring(0, 0))
        signalMargin.set(None)
        freezeVisible.set(false)
        controlsTransitioning.set(false)

    /** Starts counting over from nothing: the tally, the detector, and the signal behind them. Reached from this
      * device's own control and from a watching one, which must mean the same thing on both.
      *
      * A full wipe rather than a zeroed tally. The button exists to discard what accrued while the user was getting
      * into position, and leaving the buffer would keep that movement working against them twice over -- its peaks can
      * still be counted, and its strength still sets the threshold that the real exercise has to clear. Vigorous
      * setting-up was measured suppressing the exercise that followed for as long as the detector's window holds it.
      *
      * The cost is the fifteen seconds the detector needs before it can lock again, and it is not a loss: the reps
      * performed in the meantime are in the buffer, and the first lock counts the run it finds there.
      */
    def resetCount(): Unit =
      baseline = ResumedRun.Nothing
      signals.clear()
      counter.reset()
      totalSamples = 0
      restOffsets.set(Map.empty)
      lock.set(LockState.Acquiring(0, 0))
      signalMargin.set(None)
      // Cleared rather than saved as zero: a reload should find nothing to resume, rather than a zero that goes on being
      // resumed for the rest of the retention window.
      repCountStore.clear()
      repCount.set(0)
      // Announced rather than left to the next change of reading. A reset is the one moment a watching device needs to
      // hear about even if nothing else has moved: it is how the bench knows its command arrived, and a command silently
      // lost left one phone counting a whole test onto the previous test's total.
      lastPublished = None
      publish()

    def reportWhileActive(): Unit =
      reporter.start(() => baseline.plus(counter.reading).progress)

    def workoutSnapshot(progress: RepProgress): WorkoutSnapshot =
      WorkoutSnapshot(
        exerciseType = workoutSettings.selectedExercise.map(_.name.trim).filter(_.nonEmpty).getOrElse("Counting"),
        calories = Effort.calories(progress.reps, progress.cadenceSum, workoutSettings),
        exerciseFactor = workoutSettings.factor,
        weightKilograms = workoutSettings.weightKilograms,
        countsBy = workoutSettings.countsBy
      )

    def startWorkout(): Unit =
      if !workoutActive.now() && !workoutBusy.now() then
        workoutBusy.set(true)
        workoutSettings = settingsPanel.settings.now()
        val empty = RepProgress(0)
        workoutRequests.run(api.controlWorkout(WorkoutAction.Start, snapshot = Some(workoutSnapshot(empty)))):
          case Right(state) if state.active =>
            resetCount()
            workoutActive.set(true)
            workoutBusy.set(false)
            reportWhileActive()
            lastPublished = None
            publish()
          case Right(_) =>
            workoutBusy.set(false)
            dom.console.warn("The server did not start the workout")
          case Left(error) =>
            workoutBusy.set(false)
            dom.console.warn(error.message)

    def stopWorkout(): Unit =
      if workoutActive.now() && !workoutBusy.now() then
        workoutBusy.set(true)
        reporter.stop()
        val finalProgress = baseline.plus(counter.reading).progress
        workoutRequests.run(
          api.controlWorkout(WorkoutAction.Stop, Some(finalProgress), Some(workoutSnapshot(finalProgress)))
        ):
          case Right(state) if !state.active =>
            workoutActive.set(false)
            workoutBusy.set(false)
            lastPublished = None
            publish()
          case Right(_) =>
            workoutBusy.set(false)
            reportWhileActive()
            dom.console.warn("The server did not stop the workout")
          case Left(error) =>
            workoutBusy.set(false)
            reportWhileActive()
            dom.console.warn(error.message)

    def captureTrace(note: Option[String] = None): Unit =
      if TraceCapture.worthSending(signals) then
        TraceCapture.send(
          api.http,
          TraceCapture
            .of(
              signals,
              repCount.now(),
              lock.now(),
              note,
              cameraReport,
              controlNote,
              controlsAtSample,
              Device.describe()
            ),
          capture.set
        )
      else capture.set(CaptureState.Failed("nothing recorded yet"))

    /** Acts on what a watching device asks: a reset, or a recording of the signal.
      *
      * The acquirer is the authority throughout: a command is a request to do the same thing this device's own controls
      * do, not a way to set the count from outside.
      */
    def obey(text: String): Unit =
      text.fromJson[LiveCommand] match
        case Right(LiveCommand.Reset)              => resetCount()
        case Right(LiveCommand.Start)              => startWorkout()
        case Right(LiveCommand.Stop)               => stopWorkout()
        case Right(LiveCommand.CaptureTrace(note)) => captureTrace(note)
        case Left(details)                         => dom.console.warn(s"Ignoring an unreadable command: $details")

    /** The counting end of the direct link. It answers offers rather than making them: a counter with nobody watching
      * has nothing to offer, and a watcher appearing is what starts the exchange.
      */
    lazy val peer: PeerLink = PeerLink(
      api.http,
      PeerRole.Counter,
      obey,
      onOpen = () => dom.console.info("A watching device is reading the count directly"),
      onClosed = () => dom.console.info("The direct link closed; nothing reads the count until a watcher offers again")
    )

    def release(): Unit =
      sampler.foreach(_.stop())
      sampler = None
      stream.foreach(Camera.stop)
      stream = None
      adjustables.set(Seq.empty)
      freezeVisible.set(false)
      controlsTransitioning.set(false)
      // Stopping the tracks is not enough on its own: while the video element still holds the stream, the browser can
      // keep the camera powered and its indicator lit after the view has gone. Letting go of it here is what actually
      // turns the camera off.
      Camera.detach(video.ref)
      signals.clear()
      restOffsets.set(Map.empty)
      // Detection starts over from nothing, but what was already counted stands: switching cameras mid-set is a change
      // of viewpoint, not a new workout. Absorbing it into the baseline first is what keeps it -- the whole of it, since
      // a set whose calories reset at a camera change would be no better off than one whose reps did.
      baseline = baseline.plus(counter.reading)
      counter.reset()
      totalSamples = 0
      repCount.set(baseline.count)
      lock.set(LockState.Acquiring(0, 0))

    def startCamera(deviceId: Option[String] = None): Unit =
      if cameraState.now() != CameraState.Starting then
        cameraState.set(CameraState.Starting)
        Camera
          .start(
            deviceId,
            outcome =>
              controlNote = Some(TraceCapture.describe(outcome))
              // The sample count at the moment they settled: a trace can then be read for whether the picture changed
              // here or somewhere else entirely.
              controlsAtSample = Some(totalSamples)
            ,
            // The whole frame's brightness over the last half second, from the samples already being taken. What lets
            // the hold be checked against the picture rather than against the camera's word for it.
            brightness = () =>
              val recent = signals.window(Camera.BrightnessWindow).values.flatten
              Option.when(recent.nonEmpty)(recent.sum / recent.size)
            ,
            onTransition = controlTransition
          )
          .onComplete:
            case Success(opened) if !live =>
              // Torn down while the camera was opening: release it rather than sample into nothing.
              Camera.stop(opened)
            case Success(opened) =>
              stream = Some(opened)
              cameraReport = Camera.report(opened)
              // Read after the controls have been held, so the sliders start from what the camera actually settled on
              // rather than from what it was doing while metering was still moving.
              adjustables.set(
                (for
                  capabilities <- CameraAdjust.capabilitiesOf(opened)
                  settings <- CameraAdjust.settingsOf(opened)
                yield CameraAdjust.adjustable(capabilities, settings)).getOrElse(Seq.empty)
              )
              val element = video.ref
              prepare(element)
              CameraInterop.attach(element, opened)
              val _ = element.play()
              // Taken from the element rather than from the track, and re-taken whenever it changes. The element's
              // intrinsic size is what is actually being painted; a track's reported settings can describe the frame
              // before the device rotated it, and can still name the old mode for a moment after a constraint has been
              // applied. Either disagreement would size the box to a shape the picture does not have, and put the drawn
              // quadrant lines somewhere the detector is not sampling.
              //
              // Assigned rather than added, so restarting the camera replaces these handlers instead of stacking
              // another copy on the same element.
              val noteShape: dom.Event => Unit = _ =>
                val shown = element.videoWidth
                val tall = element.videoHeight
                if shown > 0 && tall > 0 then
                  frame.ref.style.setProperty("--frame-aspect", (shown.toDouble / tall).toString)
                  cameraState.set(CameraState.Streaming(shown, tall))
              CameraInterop.onShapeChanged(element)(noteShape)
              val (width, height) = Camera.resolution(opened).getOrElse((0, 0))
              if width > 0 && height > 0 then
                frame.ref.style.setProperty("--frame-aspect", (width.toDouble / height).toString)
              mirrored.set(Camera.mirrors(Camera.facing(opened)))
              currentDevice.set(Camera.deviceIdOf(opened))
              // Labels stay blank until permission is granted, so the list is only worth reading now.
              Camera.videoInputs().foreach(devices.set)
              cameraState.set(CameraState.Streaming(width, height))
              val started = FrameSampler(element, onSample, () => mirrored.now())
              started.start()
              sampler = Some(started)
            case Failure(error) =>
              release()
              cameraState.set(CameraState.Unavailable(Camera.failureMessage(error)))

    def graphPane(quadrant: Quadrant): Element =
      val pane = canvasTag(cls := "signal-canvas")
      div(
        cls := "signal-pane",
        // Unlabelled. The panes are laid out as the quadrants are, so where a trace sits already says which part of the
        // frame it came from -- and "Q2" says that to nobody who has not read the detector.
        // Marks the channel the count is taken from, and the channel corroborating it. One marker in one place, coloured
        // by role: a quadrant is never both at once, and two markers at different offsets made the same fact appear in
        // two different spots depending on which role it happened to be.
        div(
          cls := "channel-dot",
          cls("leader") <-- lock.signal.map:
            case LockState.Locked(channel, _, _) => channel == quadrant
            case _                               => false
          ,
          cls("partner") <-- lock.signal.map:
            case LockState.Locked(_, partner, _) => partner == quadrant
            case _                               => false
          ,
          // Decoration over the trace it belongs to, and nothing a screen reader can do anything with.
          aria.hidden := true
        ),
        pane,
        // Redraw on every sample, so the trace keeps scrolling on a still scene too: samples arrive whether or not
        // anything in front of the camera moves.
        tick.signal --> { _ =>
          val element = pane.ref
          val box = element.getBoundingClientRect()
          val ratio = dom.window.devicePixelRatio
          val width = math.max(1, (box.width * ratio).round.toInt)
          val height = math.max(1, (box.height * ratio).round.toInt)
          if element.width != width || element.height != height then
            element.width = width
            element.height = height
          val recent = signals.window(SignalGraph.WindowSamples)
          SignalGraph.draw(
            element,
            Seq(SignalGraph.Trace(recent.getOrElse(quadrant, Seq.empty), stroke = "#22c55e", width = 2)),
            grid = "rgb(128 128 128 / 35%)"
          )
        }
      )

    def menuItem(label: String, act: () => Unit): Element = Menu.item(menuOpen, label, act)

    div(
      cls := "acquirer-view",
      statusText --> { text =>
        latestStatus = text
        publish()
      },
      onMountCallback { _ =>
        // Waits to be found: a watching device offers, this end answers. Until one appears there is nothing to pair
        // with, which is why the counter only listens.
        peer.connect()
        // Before anything is reported: a report carries a count into the account's session, and until the role is taken
        // there is no session to carry it into.
        roleRequests.run(api.takeCounterRole()):
          case Left(error)  => dom.console.warn(error.message)
          case Right(state) =>
            workoutActive.set(state.active)
            if state.active then reportWhileActive() else resetCount()
            lastPublished = None
            publish()
        startCamera()
        // Leaving this screen is what releases the camera; staying would leave a phone counting into a room it no
        // longer owns, with its own tally still climbing on screen.
        reporter.onDisplaced = () => show(Screen.Selection)
      },
      onUnmountCallback { _ =>
        live = false
        roleRequests.invalidate()
        workoutRequests.invalidate()
        peer.close()
        reporter.stop()
        release()
      },
      div(
        cls := "screen acquirer",
        div(
          cls := "acquirer-header",
          h1(cls := "screen-title", "Counter"),
          Menu.toggle(menuOpen)
        ),
        Menu.backdrop(menuOpen),
        div(
          cls := "camera",
          frame,
          child <-- cameraState.signal.map:
            case CameraState.Idle     => emptyNode
            case CameraState.Starting => p(cls := "camera-status", "Waiting for camera permission…")
            // One bullet per device reading this counter directly, where the resolution used to be: a counter may be
            // read by several at once, and each is its own connection rather than a shared one.
            case CameraState.Streaming(_, _) =>
              div(
                cls := "watcher-dots",
                children <-- peer.watchers.signal.map: many =>
                  Seq.fill(many)(span(cls := "watcher-dot", aria.hidden := true))
              )
            case CameraState.Unavailable(message) =>
              div(
                cls := "camera-status",
                p(cls := "error", message),
                button(cls := "back-button", typ := "button", "Try again", onClick --> (_ => startCamera()))
              )
        ),
        Readouts.reading(repCount.signal.map(_.toString), "reps"),
        Readouts.controls(
          shownStatus,
          workoutActive.signal,
          workoutBusy.signal,
          () => startWorkout(),
          () => stopWorkout(),
          signalMargin.signal
        ),
        Menu.sheet(
          menuOpen,
          // Shown only when there is somewhere to switch to.
          child <-- devices.signal
            .combineWith(currentDevice.signal)
            .map: (available, current) =>
              Camera.nextDevice(available, current) match
                case None       => emptyNode
                case Some(next) =>
                  val name = CameraDevice.nameOf(next, available.indexWhere(_.deviceId == next.deviceId))
                  menuItem(
                    s"Switch to $name",
                    // The scene changes entirely, so the buffered signal and the count start again: what came before
                    // belongs to a different view of the world.
                    () => { release(); startCamera(Some(next.deviceId)) }
                  ),
          // Only where there is something to move. A menu entry that opens an empty sheet is worse than no entry, and
          // on Safari there is nothing to move: it reports exposure as modes, with no range to put a slider on.
          child <-- adjustables.signal.map: available =>
            if available.isEmpty then emptyNode
            else menuItem("Camera controls", () => adjustOpen.set(true)),
          // Kept in the menu rather than on the panel: capturing is for working on the detector, not for working out,
          // and a control that stops a set is worth a deliberate tap.
          child <-- capture.signal.map: state =>
            val label = state match
              case CaptureState.Idle            => "Capture signal trace"
              case CaptureState.Sending         => "Capturing…"
              case CaptureState.Captured(reps)  => s"Captured at $reps reps"
              case CaptureState.Failed(message) => s"Capture failed: $message"
            button(
              cls := "menu-item",
              typ := "button",
              label,
              disabled := state == CaptureState.Sending,
              // Deliberately does not close the menu: the outcome is reported on this very item, and closing would
              // hide the one thing the tap was for.
              onClick --> (_ => captureTrace())
            )
          ,
          menuItem("Settings", () => settingsPanel.open()),
          menuItem("About", openAbout),
          Menu.documentItems(menuOpen),
          menuItem("Back", () => show(Screen.Selection)),
          menuItem("Logout", logout)
        )
      ),
      // A sheet at the foot of the screen rather than a panel in the flow: what these sliders do is only visible in the
      // picture, so the picture has to stay on screen while they move.
      child <-- adjustOpen.signal
        .combineWith(adjustables.signal)
        .map: (open, available) =>
          if !open || available.isEmpty then emptyNode
          else
            val dial = stream.map(CameraAdjust.Dial(_))
            div(
              cls := "camera-sheet",
              div(
                cls := "camera-sheet-head",
                span("Camera"),
                button(
                  cls := "camera-sheet-close",
                  typ := "button",
                  aria.label := "Close camera controls",
                  "\u2715",
                  onClick --> (_ => adjustOpen.set(false))
                )
              ),
              available.map: control =>
                val shown = Var(control.current)
                div(
                  cls := "camera-adjust-row",
                  label(cls := "camera-adjust-label", control.label),
                  input(
                    cls := "camera-adjust-slider",
                    typ := "range",
                    minAttr := "0",
                    maxAttr := CameraAdjust.Positions.toString,
                    stepAttr := "1",
                    defaultValue := (CameraAdjust
                      .positionOf(control, control.current) * CameraAdjust.Positions).round.toString,
                    onInput.mapToValue --> { raw =>
                      raw.toDoubleOption.foreach: position =>
                        val value = CameraAdjust.valueAt(control, position / CameraAdjust.Positions)
                        shown.set(value)
                        dial.foreach(_.set(control, value))
                    }
                  ),
                  span(cls := "camera-adjust-value", child.text <-- shown.signal.map(value => f"$value%.4g"))
                )
              ,
              // The way back from a picture no slider can recover, which is how this was first met.
              button(
                cls := "camera-sheet-auto",
                typ := "button",
                "Back to automatic",
                onClick --> { _ =>
                  dial.foreach: one =>
                    val _ = one.automatic()
                  adjustOpen.set(false)
                }
              )
            )
      ,
      // Below the panel and laid out as the quadrants themselves are, so a trace sits where the movement that produced
      // it appeared on screen.
      div(
        cls := "signal-graphs",
        Seq(Quadrant.Q2, Quadrant.Q1, Quadrant.Q3, Quadrant.Q4).map(graphPane)
      )
    )
