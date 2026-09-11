package sgrv.fe.acquire

import munit.FunSuite

class SampleQueueSuite extends FunSuite:

  test("analyses fifteen seconds and keeps six minutes"):
    assertEquals(QuadrantSignals.DetectionWindow, 150)
    assertEquals(QuadrantSignals.DetectionWindow * FrameSampler.DefaultIntervalMillis, 15_000)
    // Six minutes kept, so a captured trace covers a whole bench suite rather than its closing minute.
    assertEquals(QuadrantSignals.Recorded * FrameSampler.DefaultIntervalMillis, 360_000)
    assert(QuadrantSignals.Recorded > QuadrantSignals.DetectionWindow, "keeping less than is analysed makes no sense")

  test("the window still holds enough slow reps to be counted"):
    // The window was shortened so the prominence bar stops carrying stale history. The limit on how far it can go is
    // the sustained-movement rule: after the filter's settling samples are dropped, what is left must still hold the
    // peaks that rule demands, at the slowest cadence the band-pass admits. Asserted rather than assumed, so the two
    // cannot be tuned apart.
    val settings = DetectorSettings()
    val analysed = QuadrantSignals.DetectionWindow - settings.settlingSamples
    val slowestPeriodSamples = settings.sampleRateHz / settings.lowHz
    assert(
      analysed >= settings.minimumSustainedPeaks * slowestPeriodSamples,
      s"$analysed samples cannot hold ${settings.minimumSustainedPeaks} peaks at ${settings.lowHz}Hz"
    )
    // And a lock cannot need more signal than there is signal to lock onto.
    assert(settings.minimumSamplesForLock <= QuadrantSignals.DetectionWindow)

  test("returns samples oldest first while filling"):
    val queue = SampleQueue(5)
    Seq(1.0, 2.0, 3.0).foreach(queue.add)

    assertEquals(queue.size, 3)
    assert(!queue.isFull)
    assertEquals(queue.toSeq, Seq(1.0, 2.0, 3.0))

  test("overwrites the oldest sample once full and never grows"):
    val queue = SampleQueue(3)
    Seq(1.0, 2.0, 3.0, 4.0, 5.0).foreach(queue.add)

    assert(queue.isFull)
    assertEquals(queue.size, 3)
    assertEquals(queue.toSeq, Seq(3.0, 4.0, 5.0))

  test("a window asks for the newest samples, oldest first"):
    val queue = SampleQueue(10)
    (1 to 8).foreach(value => queue.add(value.toDouble))

    assertEquals(queue.latest(3), Seq(6.0, 7.0, 8.0))
    // Asking for more than is held yields everything rather than padding.
    assertEquals(queue.latest(50), (1 to 8).map(_.toDouble))
    assertEquals(queue.latest(0), Seq.empty[Double])

  test("a window still reads in order after the ring has wrapped"):
    val queue = SampleQueue(4)
    (1 to 10).foreach(value => queue.add(value.toDouble))

    assertEquals(queue.toSeq, Seq(7.0, 8.0, 9.0, 10.0))
    assertEquals(queue.latest(2), Seq(9.0, 10.0))

  test("each quadrant is recorded into its own series"):
    val signals = QuadrantSignals(capacity = 4)
    signals.record(Sample(0, Map(Quadrant.Q1 -> 1.0, Quadrant.Q2 -> 2.0, Quadrant.Q3 -> 3.0, Quadrant.Q4 -> 4.0)))
    signals.record(Sample(100, Map(Quadrant.Q1 -> 5.0, Quadrant.Q2 -> 6.0, Quadrant.Q3 -> 7.0, Quadrant.Q4 -> 8.0)))

    assertEquals(signals.latest(Quadrant.Q1, 2), Seq(1.0, 5.0))
    assertEquals(signals.latest(Quadrant.Q4, 2), Seq(4.0, 8.0))
    assertEquals(signals.size, 2)

  test("clearing drops every series"):
    val signals = QuadrantSignals(capacity = 4)
    signals.record(Sample(0, Quadrant.All.map(_ -> 1.0).toMap))
    signals.clear()

    assertEquals(signals.size, 0)
    Quadrant.All.foreach(quadrant => assertEquals(signals.latest(quadrant, 10), Seq.empty[Double]))
