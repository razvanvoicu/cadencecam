package sgrv.fe.bench

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.{Live, LiveCommand, LiveState, TestEvent}
import sgrv.fe.acquire.StatusLine
import sgrv.fe.live.LiveSocket
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
    val plan = TestPlan.standard()
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
    val breakRemaining = Var(Option.empty[Int])

    val canvas = canvasTag(cls := "bench-canvas")

    // Read once: it cannot change while the page is open, and every event carries it.
    val device = Device.describe()

    val relay: LiveSocket = LiveSocket(
      Live.DashboardPath,
      text =>
        text.fromJson[LiveState] match
          case Right(state) =>
            state.reading.foreach: latest =>
              acquired.set(latest.reps)
              statusText.set(latest.status)
            if !state.acquiring then statusText.set("No device is counting yet")
          case Left(details) => dom.console.warn(s"Ignoring an unreadable update: $details"),
      onOpen = () => connected.set(true),
      onClosed = () => connected.set(false)
    )

    /** Sends one observation to be recorded. Fire and forget: a lost report costs a line in a log, and blocking the
      * animation to be sure of it would corrupt the very thing being measured.
      */
    def report(kind: String, at: Double, detail: Option[String] = None): Unit =
      val event = TestEvent(
        runId = runId,
        testName = stage.now() match
          case Stage.Poised(index, _)     => plan(index).name
          case Stage.Running(index, _)    => plan(index).name
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
        device = device
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
        stage.set(Stage.Poised(index, dom.window.performance.now() + TestPlan.StillBeforeMovingMillis))
        report("started", 0.0, Some(plan(index).name))

    /** Clears the counter on the other device, then waits before moving again.
      *
      * The wait matters: the command has to travel and take effect, and movement that begins before it has would be
      * credited to the test that just ended. The reset now also wipes the signal buffer, so each test is measured
      * against its own noise floor rather than against whatever the previous one left behind.
      */
    def resetThen(index: Int): Unit =
      val _ = relay.send(LiveCommand.Reset.toJson)
      val _ = dom.window.setTimeout(() => beginAt(index), Bench.SettleAfterResetMillis.toDouble)

    /** Asks for the recording of the test just finished, then resets once it has had time to arrive.
      *
      * The order is the whole point. A reset wipes the buffer the recording is made from, so capturing afterwards would
      * file an empty one -- and one trace per test is now what a suite produces, each holding only its own movement
      * rather than everything since the run began.
      */
    def captureThenReset(index: Int): Unit =
      val capture: LiveCommand = LiveCommand.CaptureTrace(Some(s"bench $runId, test ${index + 1}: ${plan(index).name}"))
      val _ = relay.send(capture.toJson)
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
            stage.set(Stage.Pausing(index, now + TestPlan.PauseSeconds * 1000, comparison.reference))
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
      .combineWith(stage.signal, reference.signal, breakRemaining.signal)
      .map: (done, current, shown, waiting) =>
        val index = current match
          case Stage.Poised(i, _)     => i + 1
          case Stage.Running(i, _)    => i + 1
          case Stage.Pausing(i, _, _) => i + 1
          // Between two tests: the one just scored is behind us, so the next is the one to name.
          case Stage.Settling(i) => math.min(i + 2, plan.size)
          case Stage.Finished    => plan.size
          case Stage.Idle        => 0
        val passed = done.count(_.passed)
        // One row, two jobs: while a test runs it counts the reps shown, and during the break it says how long is
        // left of it. Nothing is being shown then, so the reps figure is a stale number holding a useful line.
        val progress = waiting match
          case Some(seconds) => "next test in" -> s"${seconds}s"
          case None          => "reps in this test" -> shown.toString
        Seq(
          "current test" -> (if index == 0 then s"none of ${plan.size}" else s"$index of ${plan.size}"),
          progress,
          "tests completed" -> s"${done.size} of ${plan.size}",
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
    val results = outcomes.signal.map: done =>
      plan.zipWithIndex.map((test, index) => test.shortName -> done.lift(index))

    /** One line for the end of the suite, where the detail above has stopped changing. */
    val summary = outcomes.signal.map: done =>
      val passed = done.count(_.passed)
      s"${done.size} of ${plan.size} tests completed · $passed passed · ${done.size - passed} failed"

    div(
      cls := "bench-view",
      onMountCallback { _ =>
        relay.connect()
        sizeCanvas()
        val _ = dom.window.requestAnimationFrame(now => loop(now))
      },
      onUnmountCallback { _ =>
        running = false
        relay.close()
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
                "Line the camera up with the four green crosses and start it counting, then begin from the panel " +
                  f"on the right. ${plan.size} tests, about ${TestPlan.durationSeconds(plan) / 60}%.0f minutes."
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
          cls("astray") <-- withinTolerance.signal.map(!_)
        ),
        p(
          cls := "lock-state",
          child.text <-- statusText.signal
            .combineWith(connected.signal)
            .map((text, live) => if live then text else "Connecting…")
        ),
        div(
          cls := "bench-progress",
          children <-- stats.map: figures =>
            figures.map: (label, value) =>
              div(cls := "bench-stat", span(cls := "bench-stat-label", label), span(cls := "bench-stat-value", value))
        ),
        child <-- stage.signal.map:
          case Stage.Idle =>
            button(
              cls := "mode-button bench-begin",
              typ := "button",
              "Begin the suite",
              onClick --> (_ => resetThen(0))
            )
          case _ => emptyNode
        ,
        button(cls := "back-button bench-back", typ := "button", "Back", onClick --> (_ => onBack()))
      ),
      div(
        cls := "bench-results",
        div(
          cls := "bench-results-table",
          div(
            cls := "bench-result heading",
            span(cls := "bench-result-name", "test"),
            span(cls := "bench-result-number", "shown"),
            span(cls := "bench-result-number", "counted"),
            span(cls := "bench-result-number", "off")
          ),
          children <-- results.map: rows =>
            rows.map: (name, outcome) =>
              div(
                cls := "bench-result",
                cls("passed") := outcome.exists(_.passed),
                cls("failed") := outcome.exists(!_.passed),
                cls("pending") := outcome.isEmpty,
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
