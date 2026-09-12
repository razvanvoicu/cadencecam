package sgrv.fe.bench

import munit.FunSuite
import sgrv.fe.acquire.DetectorSettings

class StageSuite extends FunSuite:

  test("a test is scored exactly once, however many frames find the pause over"):
    // The bug this exists for: scoring only schedules the next test, a second and a half later, so a stage that
    // stayed on Pausing scored the same test on every frame until then -- ninety times, with ninety resets sent to
    // the other device, and "91 of 6 tests completed" on screen.
    var stage: Stage = Stage.Pausing(index = 2, until = 1000.0, reference = 30)
    var scored = List.empty[(Int, Int)]

    // A second and a half of frames at sixty a second, all after the pause ended.
    for step <- 0 until 90 do
      val (next, outcome) = Stage.onFrame(stage, 1000.0 + step * 16.7)
      stage = next
      outcome.foreach(scored :+= _)

    assertEquals(scored, List(2 -> 30))
    assertEquals(stage, Stage.Settling(2))

  test("a pause still running scores nothing and stays where it is"):
    val pausing = Stage.Pausing(index = 0, until = 1000.0, reference = 30)

    val (stage, scored) = Stage.onFrame(pausing, 999.0)

    assertEquals(stage, pausing)
    assertEquals(scored, None)

  test("the frame on which the pause ends is the one that scores"):
    val pausing = Stage.Pausing(index = 1, until = 1000.0, reference = 12)

    assertEquals(Stage.onFrame(pausing, 1000.0), (Stage.Settling(1), Some(1 -> 12)))

  test("no other stage scores anything"):
    val others = Seq(
      Stage.Idle,
      Stage.Running(0, 0.0),
      Stage.Settling(0),
      Stage.Finished,
      Stage.Poised(0, 10_000_000.0),
      Stage.Holding(0, 10_000_000.0, 100)
    )

    others.foreach: stage =>
      assertEquals(Stage.onFrame(stage, 10_000.0), (stage, None), s"$stage scored something")

  private val plan = TestPlan.standard()

  test("once the movement is over the figure leaves the frame"):
    // A set ends with the weight being put down. Without that, the last rep's peak has no trough after it, is worth
    // half the prominence of its neighbours, and goes uncounted until the threshold decays enough to admit it --
    // twenty seconds on one test and thirty on another, measured from real recordings.
    assertEquals(Stage.restingPalette(Stage.Pausing(0, 1000.0, 100), plan), Some(plan(0).palette))
    assertEquals(Stage.restingPalette(Stage.Settling(1), plan), Some(plan(1).palette))

  test("while the movement runs the figure belongs on screen"):
    assertEquals(Stage.restingPalette(Stage.Running(0, 0.0), plan), None)
    assertEquals(Stage.restingPalette(Stage.Idle, plan), None)
    assertEquals(Stage.restingPalette(Stage.Finished, plan), None)
    // And before it runs: the figure is held still there, not absent.
    assertEquals(Stage.restingPalette(Stage.Poised(0, 1000.0), plan), None)

  test("the figure is held still, then starts moving at the moment it is due"):
    // The mirror of the pause at the end. A set begins with the weight picked up and held; starting from an empty
    // frame made the figure's arrival a step change in whichever quadrants it landed in, with nothing before it to
    // be measured against.
    val poised = Stage.Poised(index = 3, until = 1000.0)

    assertEquals(Stage.onFrame(poised, 999.0), (poised, None), "it moved early")
    assertEquals(Stage.onFrame(poised, 1000.0), (Stage.Running(3, 1000.0), None))

  test("the still moment is not counted: the reference clock starts when the movement does"):
    // Timing the reps from the figure's arrival would have the harness expecting a rep the animation has not shown,
    // and scoring the counter as behind for a second it was given nothing to count.
    val (started, _) = Stage.onFrame(Stage.Poised(0, 1000.0), 1400.0)

    assertEquals(started, Stage.Running(0, 1400.0), "the clock must start now, not when the figure appeared")

  test("a countdown reaches zero when the wait is over, and never goes below it"):
    // Rounded up, so a second still to run reads as one rather than as none.
    assertEquals(Stage.secondsRemaining(Some(10_000.0), 0.0), Some(10))
    assertEquals(Stage.secondsRemaining(Some(10_000.0), 9_001.0), Some(1))
    assertEquals(Stage.secondsRemaining(Some(10_000.0), 10_000.0), Some(0))
    assertEquals(Stage.secondsRemaining(Some(10_000.0), 12_000.0), Some(0), "a countdown must not run backwards")
    assertEquals(Stage.secondsRemaining(None, 0.0), None, "nothing pending, nothing to show")

  test("the figure stands still for longer than the movement's own period"):
    // The arrival is a step and its transient reaches forward. A still moment shorter than a rep leaves the first
    // crossing inside that transient, where it is lost -- and it is lost in whichever quadrant happens to fall
    // closest, which then leads the count one short. Asserted against the slowest cadence the band admits, so
    // shortening either of them cannot quietly reintroduce it.
    val slowestPeriodMillis = 1000.0 / DetectorSettings().lowHz

    assert(
      TestPlan.StillBeforeMovingMillis >= slowestPeriodMillis,
      s"${TestPlan.StillBeforeMovingMillis}ms is shorter than the ${slowestPeriodMillis}ms a rep can take"
    )

  test("the figure is held where it finished before it is put down"):
    // The mirror of the hold at the start, and for the same reason: the movement ending and the object leaving are
    // two events, and run together the last rep's own return is tangled with the step of the object going.
    val holding = Stage.Holding(index = 2, until = 5000.0, reference = 100)

    assertEquals(Stage.onFrame(holding, 4999.0), (holding, None), "it was put down early")
    val (next, scored) = Stage.onFrame(holding, 5000.0)
    assertEquals(scored, None, "nothing is scored when the figure goes; that waits for the pause to end")
    assertEquals(next, Stage.Pausing(2, 5000.0 + TestPlan.PauseSeconds * 1000, 100))

  test("while the figure is still being held it belongs on screen"):
    assertEquals(Stage.restingPalette(Stage.Holding(0, 1000.0, 100), plan), None)

  test("the hold at each end is the same length, so the two ends can be compared"):
    assertEquals(TestPlan.StillBeforeMovingMillis, TestPlan.StillAfterMovingMillis)

  test("the break lasts long enough for everything that has to happen in it"):
    // A capture has to reach the backend before the reset wipes the buffer it was made from, and the reset has to
    // reach the other device before the next test's movement is credited to it.
    assertEquals(TestPlan.PauseSeconds, 15)
    assertEquals(
      TestPlan.BreakMillis,
      TestPlan.StillAfterMovingMillis + TestPlan.PauseSeconds * 1000 +
        Bench.CaptureBeforeResetMillis + Bench.SettleAfterResetMillis
    )
    assert(TestPlan.BreakMillis > TestPlan.PauseSeconds * 1000, "the break outlasts the pause inside it")

  test("a stage naming a test the plan does not have asks for nothing"):
    assertEquals(Stage.restingPalette(Stage.Settling(plan.size), plan), None)
