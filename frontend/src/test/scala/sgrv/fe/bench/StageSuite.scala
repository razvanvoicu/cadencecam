package sgrv.fe.bench

import munit.FunSuite

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
    val others = Seq(Stage.Idle, Stage.Running(0, 0.0), Stage.Settling(0), Stage.Finished)

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

  test("a stage naming a test the plan does not have asks for nothing"):
    assertEquals(Stage.restingPalette(Stage.Settling(plan.size), plan), None)
