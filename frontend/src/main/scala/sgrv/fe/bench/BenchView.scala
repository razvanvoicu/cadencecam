package sgrv.fe.bench

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.{DiscardRun, LiveCommand, LiveState, PeerRole, TestEvent}
import sgrv.fe.acquire.StatusLine
import sgrv.fe.live.PeerLink
import sgrv.fe.{Device, HttpService, Readouts}
import zio.json.*

import scala.scalajs.js

/** A dashboard that also produces the movement being counted.
  *
  * The point is a known truth. Everything measured so far has been measured against a synthetic signal invented to
  * match a theory, or against a mechanical counter held in a hand; both have been wrong. This shows a movement whose
  * rep count is exact by construction, and compares the detector's answer against it continuously rather than only at
  * the end.
  *
  * To the backend it is an ordinary dashboard: it watches the same readings and sends the same commands. It runs on a
  * desktop screen with the acquiring device on a tripod pointed at the canvas.
  */
private[fe] object BenchView:

  def apply(http: HttpService, onBack: () => Unit): Element =
    val catalogue = TestPlan.standard()
    // Which of the catalogue's tests are ticked. Read when a suite begins and fixed for its length, so unticking a box
    // part way through cannot change what the running suite is.
    val included = Var(catalogue.indices.toSet)
    var suite = TestPlan.chosen(catalogue, catalogue.indices.toSet)
    def plan: Seq[TestCase] = suite.map(_._2)
    val runId = f"${js.Date.now().toLong}%d-${(js.Math.random() * 4096).toInt}%03x"

    val stage = Var[Stage](Stage.Idle)
    val acquired = Var(0)
    val reference = Var(0)
    val withinTolerance = Var(true)
    val connected = Var(false)
    val outcomes = Var(Vector.empty[Outcome])
    val statusText = Var(StatusLine.Waiting)

    var stall = StallWatch()
    var wasWithin = true
    var lastComparison = Comparison(0, 0, 0.0, withinTolerance = true)
    // When the next test's figure appears, while a break is running. A plain var, read every frame; only the whole
    // second derived from it reaches the view, so the readout changes once a second rather than sixty times.
    var nextTestAt = Option.empty[Double]
    // Where the figure stopped, so it can be held there without jumping.
    var restingPhase = 0.0
    // Whether the counting device has said it is back at zero since the last reset was sent.
    var resetConfirmed = false
    // Whether it has a cadence yet. Until it does, being behind is not a discrepancy but the ordinary opening of
    // every set, and saying so in red teaches whoever is watching to ignore the colour.
    val hasCadence = Var(false)
    val breakRemaining = Var(Option.empty[Int])

    // Which handset is counting and which of its cameras is aimed at the screen. Chosen here because neither can be
    // read from the counting device: its user-agent names an engine and an OS, Chrome reports every Android model as
    // "K", and nothing in it says which camera was opened.
    val rigDevice = Var(Rig.DefaultDevice)
    val rigCamera = Var(Rig.DefaultCamera)

    val canvas = canvasTag(cls := "bench-canvas")

    // The device that is counting, which arrives with its readings. The bench's own device is a laptop showing an
    // animation; the handset whose camera and processor decide whether the reps are found is the one worth naming,
    // and the first run with this field recorded the laptop.
    val countingDevice = Var(Option.empty[String])
    val benchDevice = Device.describe()

    def readUpdate(text: String): Unit =
      text.fromJson[LiveState] match
        case Right(state) =>
          state.reading.foreach: latest =>
            acquired.set(latest.reps)
            statusText.set(latest.status)
            latest.device.foreach(name => countingDevice.set(Some(name)))
            // A zero from the counting device is how a reset is known to have landed. It announces one whether or
            // not anything else changed, so the absence of this is real evidence that the command went missing.
            if latest.reps == 0 then resetConfirmed = true
            hasCadence.set(latest.counting)
          if !state.acquiring then statusText.set("No device is counting yet")
        case Left(details) => dom.console.warn(s"Ignoring an unreadable update: $details")

    /** The bench watches a counting device exactly as a dashboard does, so it takes the same direct link.
      *
      * It matters more here than on a dashboard: a suite is scored by comparing what was shown against what was
      * counted, and every hop between the two devices is time the comparison has to allow for.
      */
    val peer: PeerLink =
      PeerLink(
        http,
        PeerRole.Watcher,
        readUpdate,
        onOpen = () => connected.set(true),
        onClosed = () => connected.set(false)
      )

    /** Asks the counting device to do something, by whichever path exists. */
    def instruct(command: LiveCommand): Unit =
      val _ = peer.send(command.toJson)

    /** Sends one observation to be recorded. Fire and forget: a lost report costs a line in a log, and blocking the
      * animation to be sure of it would corrupt the very thing being measured.
      */
    def report(kind: String, at: Double, detail: Option[String] = None): Unit =
      val event = TestEvent(
        runId = runId,
        testName = stage.now() match
          case Stage.Poised(index, _)     => plan(index).name
          case Stage.Running(index, _)    => plan(index).name
          case Stage.Holding(index, _, _) => plan(index).name
          case Stage.Pausing(index, _, _) => plan(index).name
          case Stage.Settling(index)      => plan(index).name
          case _                          => "suite"
        ,
        kind = kind,
        reference = lastComparison.reference,
        acquired = lastComparison.acquired,
        lagSeconds = lastComparison.lagSeconds,
        atSeconds = at,
        detail = detail,
        // Which handset this was. Suites from one account have come back exactly right, one short on every test, and
        // fifteen short, and nothing in the record said which phone was which.
        device = countingDevice.now().orElse(benchDevice.map(name => s"bench $name"))
      )
      val init = new dom.RequestInit:
        method = dom.HttpMethod.POST
        headers = js.Dictionary("Content-Type" -> "application/json")
        body = event.toJson
      val _ = http.send(TestEvent.Path, init)

    def beginAt(index: Int): Unit =
      if index >= plan.size then
        stage.set(Stage.Finished)
        report("suite-finished", 0.0)
      else
        acquired.set(0)
        reference.set(0)
        withinTolerance.set(true)
        wasWithin = true
        stall = StallWatch()
        lastComparison = Comparison(0, 0, 0.0, withinTolerance = true)
        nextTestAt = None
        breakRemaining.set(None)
        // The same clock the animation frame reports, which is milliseconds since the page loaded rather than
        // since 1970. Mixing the two put the elapsed time at minus fifty years and overflowed the count.
        // The figure stands still first; the reference clock starts when it begins to move.
        // A new test has no cadence yet, whatever the last one ended on, so the count goes neutral until the
        // counting device says it has found one again.
        hasCadence.set(false)
        stage.set(Stage.Poised(index, dom.window.performance.now() + TestPlan.StillBeforeMovingMillis))
        report("started", 0.0, Some(plan(index).name))

    /** Clears the counter on the other device, then waits before moving again.
      *
      * The wait matters: the command has to travel and take effect, and movement that begins before it has would be
      * credited to the test that just ended. The reset now also wipes the signal buffer, so each test is measured
      * against its own noise floor rather than against whatever the previous one left behind.
      */
    def resetThen(index: Int, attempt: Int = 1): Unit =
      resetConfirmed = false
      instruct(LiveCommand.Reset)
      val _ = dom.window.setTimeout(
        () =>
          if resetConfirmed then beginAt(index)
          else if attempt < Bench.ResetAttempts then
            // Sent and never acknowledged. Beginning anyway is how a test came to count a hundred and ninety-nine,
            // and how another counted nothing at all for two minutes and passed, so it is worth asking again.
            report("reset-unconfirmed", 0.0, Some(s"attempt $attempt found no zero from the counting device"))
            resetThen(index, attempt + 1)
          else
            // Out of attempts. The test runs regardless -- refusing to start would lose the rest of the suite -- but
            // the recording says plainly that its starting point was never confirmed.
            report("reset-failed", 0.0, Some(s"no zero after ${Bench.ResetAttempts} attempts; the count may be stale"))
            beginAt(index)
        ,
        Bench.SettleAfterResetMillis.toDouble
      )

    /** Stops the suite where it stands and unfiles everything it has written.
      *
      * For a run that has gone wrong while it is still going wrong -- a notification, a knocked camera, a phone that
      * stopped counting -- rather than one that merely scored badly. What it leaves behind is indistinguishable from a
      * suite that was never begun, which is the point: a partial run kept alongside whole ones is read later as a whole
      * one, and that is how a corrupt set of numbers gets into an average.
      *
      * The animation stops first. The reset and the discard both travel, and a figure still moving while they do would
      * have the counting device credit reps to a run that no longer exists.
      */
    def abandon(): Unit =
      stage.set(Stage.Idle)
      nextTestAt = None
      breakRemaining.set(None)
      outcomes.set(Vector.empty)
      acquired.set(0)
      reference.set(0)
      withinTolerance.set(true)
      wasWithin = true
      hasCadence.set(false)
      stall = StallWatch()
      lastComparison = Comparison(0, 0, 0.0, withinTolerance = true)
      statusText.set(StatusLine.Waiting)
      instruct(LiveCommand.Reset)
      val init = new dom.RequestInit:
        method = dom.HttpMethod.POST
        headers = js.Dictionary("Content-Type" -> "application/json")
        body = DiscardRun(runId).toJson
      val _ = http.send(DiscardRun.Path, init)

    /** Asks for the recording of the test just finished, then resets once it has had time to arrive.
      *
      * The order is the whole point. A reset wipes the buffer the recording is made from, so capturing afterwards would
      * file an empty one -- and one trace per test is now what a suite produces, each holding only its own movement
      * rather than everything since the run began.
      */
    def captureThenReset(index: Int): Unit =
      val rig = Rig.describe(rigDevice.now(), rigCamera.now())
      val capture: LiveCommand =
        LiveCommand.CaptureTrace(Some(s"bench $runId, test ${suite(index)._1 + 1}: ${plan(index).name}, $rig"))
      instruct(capture)
      report("trace-requested", TestPlan.PauseSeconds.toDouble, Some(plan(index).name))
      val _ = dom.window.setTimeout(() => resetThen(index + 1), Bench.CaptureBeforeResetMillis.toDouble)

    def finish(index: Int, expected: Int): Unit =
      val counted = acquired.now()
      val passed = counted == expected
      outcomes.update(_ :+ Outcome(plan(index).name, expected, counted, passed))
      report(
        if passed then "passed" else "failed",
        TestPlan.PauseSeconds.toDouble,
        Some(s"reference $expected, acquired $counted")
      )
      captureThenReset(index)

    def onFrame(now: Double): Unit =
      val remaining = Stage.secondsRemaining(nextTestAt, now)
      if remaining != breakRemaining.now() then breakRemaining.set(remaining)
      stage.now() match
        case Stage.Holding(index, _, expected) =>
          // Still there, motionless, at the phase the last rep ended on. The object has not been put down yet.
          Painter.draw(canvas.ref, plan(index), restingPhase)
          lastComparison = lastComparison.copy(acquired = acquired.now())
          withinTolerance.set(acquired.now() == expected)
          val (next, _) = Stage.onFrame(stage.now(), now)
          stage.set(next)
        case Stage.Poised(index, _) =>
          // On screen and motionless. Drawn at the phase the movement will start from, so nothing jumps when it does.
          Painter.draw(canvas.ref, plan(index), 0.0)
          val (next, _) = Stage.onFrame(stage.now(), now)
          stage.set(next)
        case Stage.Running(index, startedAt) =>
          val test = plan(index)
          val elapsed = (now - startedAt) / 1000.0
          Painter.draw(canvas.ref, test, test.cadence.phaseAt(elapsed))
          val comparison = Discrepancy.compare(test.cadence, elapsed, acquired.now(), TestPlan.ToleranceSeconds)
          lastComparison = comparison
          reference.set(comparison.reference)
          withinTolerance.set(comparison.withinTolerance)
          stall
            .observe(comparison, elapsed)
            .foreach:
              case StallWatch.Stalled(behind) =>
                report("stalled", elapsed, Some(f"counter stopped while $behind%.1fs behind"))
              case StallWatch.Recovered(seconds, during, credited, shortfall) =>
                report(
                  "recovered",
                  elapsed,
                  Some(
                    f"stalled ${seconds}%.1fs; $during reps happened, $credited credited, " +
                      (if shortfall == 0 then "none lost" else s"$shortfall lost")
                  )
                )
          // Reported on the edge rather than every frame: sixty identical lines a second would bury the moment it
          // went wrong, which is the thing being looked for.
          if comparison.withinTolerance != wasWithin then
            report(if comparison.withinTolerance then "recovered-tolerance" else "discrepancy", elapsed)
            wasWithin = comparison.withinTolerance
          if comparison.reference >= test.reps then
            report("movement-finished", elapsed, Some(s"${test.reps} reps shown"))
            // Held where it finished, then put down. The phase is kept so nothing jumps as it stops.
            restingPhase = test.cadence.phaseAt(elapsed)
            stage.set(Stage.Holding(index, now + TestPlan.StillAfterMovingMillis, comparison.reference))
            // Only when something follows. After the last test the wait leads to a summary, not to another test.
            if index + 1 < plan.size then nextTestAt = Some(now + TestPlan.BreakMillis)
        case current @ Stage.Pausing(index, _, expected) =>
          // The figure is gone, not merely stopped: a set ends with the weight being put down, and the object
          // leaving the frame is what gives the last rep the trough that every other rep had.
          Stage.restingPalette(current, plan).foreach(Painter.clear(canvas.ref, _))
          lastComparison = lastComparison.copy(acquired = acquired.now())
          // Judged against the reference now that nothing is moving. While a test runs the colour tracks how far
          // behind the counter is in seconds, which is the right question then and the wrong one afterwards: a test
          // that ends on the correct number was finishing in red, because the last rep arrived late and the lag it
          // arrived with was never revisited.
          withinTolerance.set(acquired.now() == expected)
          // The stage moves on first, so this is the only frame that scores this test.
          val (next, scored) = Stage.onFrame(stage.now(), now)
          stage.set(next)
          scored.foreach((finished, expected) => finish(finished, expected))
        case current @ Stage.Settling(_) =>
          // Still nothing there: the reset is travelling to the other device and the next test has not begun.
          Stage.restingPalette(current, plan).foreach(Painter.clear(canvas.ref, _))
        case Stage.Idle =>
          // Something to aim at. The camera is framed before the suite begins, which is exactly when the canvas used
          // to be blank, and a blank canvas gives nothing to line the quadrants up against.
          Painter.idle(canvas.ref)
        case _ => ()

    var running = true
    def loop(now: Double): Unit =
      if running then
        onFrame(now)
        val _ = dom.window.requestAnimationFrame(now => loop(now))

    def sizeCanvas(): Unit =
      val element = canvas.ref
      val box = element.getBoundingClientRect()
      val ratio = dom.window.devicePixelRatio
      val width = math.max(1, (box.width * ratio).round.toInt)
      val height = math.max(1, (box.height * ratio).round.toInt)
      if element.width != width || element.height != height then
        element.width = width
        element.height = height

    /** The five figures, each against its own label.
      *
      * Laid out rather than run together in a sentence: "18 reps shown, 1 completed, 1 passed" invites the last three
      * to be read as reps, which is the wrong unit for all of them. Completed, passed and failed count whole tests;
      * only one line here counts reps, and it says so.
      */
    val stats = outcomes.signal
      .combineWith(stage.signal, reference.signal, breakRemaining.signal, included.signal)
      .map: (done, current, shown, waiting, ticked) =>
        // Before a suite starts, the size is what is ticked; once it starts, what was ticked when it began.
        val total = if current == Stage.Idle then ticked.size else plan.size
        val index = current match
          case Stage.Poised(i, _)     => i + 1
          case Stage.Running(i, _)    => i + 1
          case Stage.Holding(i, _, _) => i + 1
          case Stage.Pausing(i, _, _) => i + 1
          // Between two tests: the one just scored is behind us, so the next is the one to name.
          case Stage.Settling(i) => math.min(i + 2, total)
          case Stage.Finished    => total
          case Stage.Idle        => 0
        val passed = done.count(_.passed)
        // One row, two jobs: while a test runs it counts the reps shown, and during the break it says how long is
        // left of it. Nothing is being shown then, so the reps figure is a stale number holding a useful line.
        val progress = waiting match
          case Some(seconds) => "next test in" -> s"${seconds}s"
          case None          => "reps in this test" -> shown.toString
        Seq(
          "current test" -> (if index == 0 then s"none of $total" else s"$index of $total"),
          progress,
          "tests completed" -> s"${done.size} of $total",
          "tests passed" -> passed.toString,
          "tests failed" -> (done.size - passed).toString
        )

    /** Every test of the plan against what it scored, filled in as the suite goes.
      *
      * The whole plan from the start rather than a list that grows: a suite takes a quarter of an hour, and which tests
      * are still to come is as much a part of reading the screen as which have finished. A test that has not run yet
      * holds its place with dashes.
      *
      * This is what a failure has to be read from. The final tally says two of six failed; only the rows say it was
      * both bars, or everything lighter, or one figure in both themes -- and those are different diagnoses.
      */
    val results = outcomes.signal
      .combineWith(included.signal)
      .map: (done, ticked) =>
        catalogue.zipWithIndex.map: (test, at) =>
          // A result belongs to the catalogue row it was run for, found by where that test sat in the suite.
          val scored = suite.indexWhere(_._1 == at) match
            case -1       => None
            case position => done.lift(position)
          (at, test.shortName, ticked(at), scored)

    /** One line for the end of the suite, where the detail above has stopped changing. */
    val summary = outcomes.signal.map: done =>
      val passed = done.count(_.passed)
      s"${done.size} of ${plan.size} tests completed · $passed passed · ${done.size - passed} failed"

    div(
      cls := "bench-view",
      onMountCallback { _ =>
        peer.connect()
        sizeCanvas()
        val _ = dom.window.requestAnimationFrame(now => loop(now))
      },
      onUnmountCallback { _ =>
        running = false
        peer.close()
      },
      windowEvents(_.onResize) --> (_ => sizeCanvas()),
      div(
        cls := "bench-stage",
        canvas,
        // Text only. Every control lives in the column beside the panel, because reaching for one puts a hand and
        // a pointer in front of the camera, and the detector counts what the camera sees.
        child <-- stage.signal.map:
          case Stage.Idle =>
            div(
              cls := "bench-overlay",
              p(
                child.text <-- included.signal.map: ticked =>
                  val tests = TestPlan.chosen(catalogue, ticked).map(_._2)
                  "Line the camera up with the four green crosses and start it counting, then begin from the panel " +
                    f"on the right. ${tests.size} tests, about ${TestPlan.durationSeconds(tests) / 60}%.0f minutes."
              )
            )
          case Stage.Finished => div(cls := "bench-overlay", p(child.text <-- summary))
          case _              => emptyNode
      ),
      div(
        cls := "bench-readouts",
        // The truth, in white: what was actually shown.
        Readouts.reading(reference.signal.map(_.toString), "shown", cls := "bench-reference"),
        // The answer, coloured by whether it is currently defensible.
        Readouts.reading(
          acquired.signal.map(_.toString),
          "counted",
          cls := "bench-acquired",
          cls("astray") <-- withinTolerance.signal
            .combineWith(hasCadence.signal)
            .map((within, counting) => counting && !within)
        ),
        p(
          cls := "lock-state",
          child.text <-- statusText.signal
            .combineWith(connected.signal)
            .map((text, live) => if live then text else "Connecting…")
        ),
        p(
          cls := "link-state",
          child.text <-- peer.phase.signal.map:
            case "direct"     => "Reading the count directly from the counting device"
            case "connecting" => "Linking to the counting device…"
            case "failed"     => "No direct link; reading through the server"
            case _            => "Reading through the server"
        ),
        div(
          cls := "bench-progress",
          children <-- stats.map: figures =>
            figures.map: (label, value) =>
              div(cls := "bench-stat", span(cls := "bench-stat-label", label), span(cls := "bench-stat-value", value))
        ),
        // What is being measured, above the control that starts it. Locked once a suite is running: the answer is
        // written into every capture the suite files, and a suite whose recordings disagree about which handset made
        // them is worse than one with the wrong handset named throughout.
        div(
          cls := "bench-choices",
          select(
            cls := "bench-choice",
            disabled <-- stage.signal.map(_ != Stage.Idle),
            value <-- rigDevice.signal,
            onChange.mapToValue --> rigDevice.writer,
            Rig.Devices.map(name => option(value := name, name))
          ),
          select(
            cls := "bench-choice",
            disabled <-- stage.signal.map(_ != Stage.Idle),
            value <-- rigCamera.signal,
            onChange.mapToValue --> rigCamera.writer,
            Rig.Cameras.map(name => option(value := name, name))
          )
        ),
        // Side by side and the same size. Two controls of different shapes stacked one above the other read as a
        // primary action and an afterthought, which is not the relationship: one starts a quarter of an hour of
        // measurement and the other leaves.
        div(
          cls := "bench-controls",
          button(
            cls := "bench-control",
            typ := "button",
            disabled <-- stage.signal
              .combineWith(included.signal)
              .map((current, ticked) => current != Stage.Idle || ticked.isEmpty),
            // One word: the pair share a row, and "Begin the suite" wrapped onto two lines while "Back" sat on one,
            // which is the uneven look this was meant to fix. The panel beside it says what is being begun.
            "Begin",
            onClick --> { _ =>
              suite = TestPlan.chosen(catalogue, included.now())
              resetThen(0)
            }
          ),
          button(cls := "bench-control", typ := "button", "Back", onClick --> (_ => onBack())),
          // Only while there is something to abandon. Offered before a suite begins it would be a button that throws
          // away the previous run's evidence, which is not what anyone reaching for it at that moment would mean.
          child <-- stage.signal.map: current =>
            if current == Stage.Idle then emptyNode
            else button(cls := "bench-control", typ := "button", "Abandon", onClick --> (_ => abandon()))
        )
      ),
      div(
        cls := "bench-results",
        div(
          cls := "bench-results-table",
          div(
            cls := "bench-result heading",
            span(cls := "bench-result-pick", ""),
            span(cls := "bench-result-name", "test"),
            span(cls := "bench-result-number", "shown"),
            span(cls := "bench-result-number", "counted"),
            span(cls := "bench-result-number", "off")
          ),
          children <-- results.map: rows =>
            rows.map: (at, name, ticked, outcome) =>
              div(
                cls := "bench-result",
                cls("passed") := outcome.exists(_.passed),
                cls("failed") := outcome.exists(!_.passed),
                cls("pending") := outcome.isEmpty,
                cls("excluded") := !ticked && outcome.isEmpty,
                span(
                  cls := "bench-result-pick",
                  input(
                    typ := "checkbox",
                    checked := ticked,
                    aria.label := s"Include $name",
                    // Only between suites: what is running was fixed when it began.
                    disabled <-- stage.signal.map(_ != Stage.Idle),
                    onChange.mapToChecked --> { on =>
                      included.update(set => if on then set + at else set - at)
                    }
                  )
                ),
                span(cls := "bench-result-name", name),
                span(cls := "bench-result-number", outcome.map(_.reference.toString).getOrElse("—")),
                span(cls := "bench-result-number", outcome.map(_.acquired.toString).getOrElse("—")),
                // Signed, and explicitly so: "4" leaves it to be worked out whether four reps were missed or
                // invented, and those are opposite faults.
                span(
                  cls := "bench-result-number",
                  outcome.map(scored => f"${scored.acquired - scored.reference}%+d").getOrElse("—")
                )
              )
        )
      )
    )
