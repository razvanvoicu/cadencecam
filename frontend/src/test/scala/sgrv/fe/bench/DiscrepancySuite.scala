package sgrv.fe.bench

import munit.FunSuite

class DiscrepancySuite extends FunSuite:

  private val steady = Cadence(hz = 1.0, swingHz = 0.0, swingEveryHz = 0.0)
  private val tolerance = TestPlan.ToleranceSeconds

  test("a counter keeping up is within tolerance and lagging by nothing"):
    val comparison = Discrepancy.compare(steady, elapsedSeconds = 10.0, acquired = 10, tolerance)

    assertEquals(comparison.reference, 10)
    assertEqualsDouble(comparison.lagSeconds, 0.0, 1e-9)
    assert(comparison.withinTolerance)

  test("lag is measured in seconds of movement, not in reps"):
    // The same shortfall means different things at different cadences, so one rep behind is not one thing. At one
    // hertz, three reps behind is three seconds.
    val behind = Discrepancy.compare(steady, elapsedSeconds = 10.0, acquired = 7, tolerance)

    assertEqualsDouble(behind.lagSeconds, 2.0, 1e-6)
    assert(behind.behind)
    assert(behind.withinTolerance, "two seconds is exactly the tolerance, so it is not yet a discrepancy")

  test("the same shortfall in reps is tolerable at speed and not at a crawl"):
    val fast = Cadence(hz = 2.0, swingHz = 0.0, swingEveryHz = 0.0)
    val slow = Cadence(hz = 0.4, swingHz = 0.0, swingEveryHz = 0.0)

    // Three reps behind: a second and a half at two hertz, seven and a half at four tenths.
    assert(Discrepancy.compare(fast, 10.0, 17, tolerance).withinTolerance)
    assert(!Discrepancy.compare(slow, 10.0, 1, tolerance).withinTolerance)

  test("a counter that has run ahead is reported as ahead"):
    val ahead = Discrepancy.compare(steady, elapsedSeconds = 10.0, acquired = 13, tolerance)

    assert(ahead.ahead)
    assertEqualsDouble(ahead.lagSeconds, -3.0, 1e-6)
    assert(!ahead.withinTolerance)

  test("a stall is noticed once the counter has been still for longer than the tolerance"):
    val watch = StallWatch()

    // Keeping up: nothing to report.
    assertEquals(watch.observe(Discrepancy.compare(steady, 5.0, 5, tolerance), 5.0), None)
    // Stopped, and now more than two seconds behind.
    val noticed = watch.observe(Discrepancy.compare(steady, 8.5, 5, tolerance), 8.5)

    assert(noticed.exists(_.isInstanceOf[StallWatch.Stalled]), s"expected a stall, got $noticed")

  test("a stall that recovers reports whether the missed reps were credited"):
    // The case the whole harness exists for: a counter that stops, then starts again. Whether it makes up what it
    // missed, silently drops it, or invents extra is invisible in a final total.
    val watch = StallWatch()
    watch.observe(Discrepancy.compare(steady, 5.0, 5, tolerance), 5.0)
    watch.observe(Discrepancy.compare(steady, 12.0, 5, tolerance), 12.0)

    // Eight reps happened while it was still; it credits five of them.
    val recovered = watch.observe(Discrepancy.compare(steady, 13.0, 10, tolerance), 13.0)

    recovered match
      case Some(StallWatch.Recovered(_, during, credited, shortfall)) =>
        assertEquals(during, 8)
        assertEquals(credited, 5)
        assertEquals(shortfall, 3)
      case other => fail(s"expected a recovery, got $other")

  test("a stall made good in full reports no shortfall"):
    val watch = StallWatch()
    watch.observe(Discrepancy.compare(steady, 5.0, 5, tolerance), 5.0)
    watch.observe(Discrepancy.compare(steady, 12.0, 5, tolerance), 12.0)

    val recovered = watch.observe(Discrepancy.compare(steady, 12.5, 12, tolerance), 12.5)

    assertEquals(recovered.collect { case r: StallWatch.Recovered => r.shortfall }, Some(0))

  test("a counter ticking along never reports a stall"):
    val watch = StallWatch()
    val events = (1 to 40).flatMap: second =>
      watch.observe(Discrepancy.compare(steady, second.toDouble, second, tolerance), second.toDouble)

    assertEquals(events, Seq.empty)
