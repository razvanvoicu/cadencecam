package sgrv.fe.acquire

import munit.FunSuite

class StatusLineSuite extends FunSuite:

  test("the words for each thing the detector can be doing"):
    assertEquals(StatusLine.of(LockState.Acquiring(0, 0)), "Waiting for the camera…")
    assertEquals(StatusLine.of(LockState.Acquiring(50, 150)), "Finding a cadence… about 10s")
    assertEquals(StatusLine.of(LockState.Searching), "No steady cadence — paused")
    assertEquals(StatusLine.of(LockState.Locked(Quadrant.Q4, Quadrant.Q2, 1.04)), "Counting Q4+Q2 · 1.0s/rep")

  test("a countdown never runs backwards past zero"):
    assertEquals(StatusLine.of(LockState.Acquiring(200, 150)), "Finding a cadence… about 0s")

  test("a watching screen says it is waiting rather than showing a count it does not have"):
    assert(StatusLine.Waiting.nonEmpty)
