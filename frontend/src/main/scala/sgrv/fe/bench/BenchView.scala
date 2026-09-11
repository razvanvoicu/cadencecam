package sgrv.fe.bench

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import sgrv.api.{Live, LiveCommand, LiveState, TestEvent}
import sgrv.fe.acquire.StatusLine
import sgrv.fe.live.LiveSocket
import sgrv.fe.{HttpService, Readouts}
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

    val canvas = canvasTag(cls := "bench-canvas")

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
        detail = detail
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
        // The recording is the point of the run, so it is taken without anyone having to remember to. The buffer
        // holds six minutes and a suite takes a little over five, so this one capture carries both tests and the
        // break between them -- the signal that counted and the signal that did not, under the same conditions.
        val _ = relay.send(LiveCommand.CaptureTrace.toJson)
        report("trace-requested", 0.0, Some(f"suite ran ${TestPlan.durationSeconds(plan)}%.0fs"))
      else
        acquired.set(0)
        reference.set(0)
        withinTolerance.set(true)
        wasWithin = true
        stall = StallWatch()
        lastComparison = Comparison(0, 0, 0.0, withinTolerance = true)
        // The same clock the animation frame reports, which is milliseconds since the page loaded rather than
        // since 1970. Mixing the two put the elapsed time at minus fifty years and overflowed the count.
        stage.set(Stage.Running(index, dom.window.performance.now()))
        report("started", 0.0, Some(plan(index).name))

    /** Clears the counter on the other device, then waits before moving again.
      *
      * The wait matters: the command has to travel and take effect, and movement that begins before it has would be
      * credited to the test that just ended.
      */
    def resetThen(index: Int): Unit =
      val _ = relay.send(LiveCommand.Reset.toJson)
      val _ = dom.window.setTimeout(() => beginAt(index), Bench.SettleAfterResetMillis.toDouble)

    def finish(index: Int, expected: Int): Unit =
      val counted = acquired.now()
      val passed = counted == expected
      outcomes.update(_ :+ Outcome(plan(index).name, expected, counted, passed))
      report(
        if passed then "passed" else "failed",
        TestPlan.PauseSeconds.toDouble,
        Some(s"reference $expected, acquired $counted")
      )
      resetThen(index + 1)

    def onFrame(now: Double): Unit =
      stage.now() match
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
        case Stage.Pausing(index, _, _) =>
          // Held still, so nothing on screen can be counted while the last reports arrive.
          Painter.draw(canvas.ref, plan(index), 0.0)
          lastComparison = lastComparison.copy(acquired = acquired.now())
          // The stage moves on first, so this is the only frame that scores this test.
          val (next, scored) = Stage.onFrame(stage.now(), now)
          stage.set(next)
          scored.foreach((finished, expected) => finish(finished, expected))
        case Stage.Settling(justFinished) =>
          // Still nothing moving: the reset is travelling to the other device and the next test has not begun.
          Painter.draw(canvas.ref, plan(justFinished), 0.0)
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
      .combineWith(stage.signal, reference.signal)
      .map: (done, current, shown) =>
        val index = current match
          case Stage.Running(i, _)    => i + 1
          case Stage.Pausing(i, _, _) => i + 1
          // Between two tests: the one just scored is behind us, so the next is the one to name.
          case Stage.Settling(i) => math.min(i + 2, plan.size)
          case Stage.Finished    => plan.size
          case Stage.Idle        => 0
        val passed = done.count(_.passed)
        Seq(
          "current test" -> (if index == 0 then s"none of ${plan.size}" else s"$index of ${plan.size}"),
          "reps in this test" -> shown.toString,
          "tests completed" -> s"${done.size} of ${plan.size}",
          "tests passed" -> passed.toString,
          "tests failed" -> (done.size - passed).toString
        )

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
        child <-- stage.signal.map:
          case Stage.Idle =>
            div(
              cls := "bench-overlay",
              p("Point the acquiring device at this panel, start it counting, then begin."),
              button(
                cls := "mode-button",
                typ := "button",
                "Begin the suite",
                onClick --> (_ => resetThen(0))
              )
            )
          case Stage.Finished =>
            div(
              cls := "bench-overlay",
              p(child.text <-- summary),
              button(cls := "mode-button", typ := "button", "Back", onClick --> (_ => onBack()))
            )
          case _ => emptyNode
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
        button(cls := "back-button bench-back", typ := "button", "Back", onClick --> (_ => onBack()))
      )
    )
