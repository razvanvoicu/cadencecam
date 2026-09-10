package sgrv.fe

import com.raquo.laminar.api.L.*
import munit.FunSuite

class ReadoutsSuite extends FunSuite:

  test("a pace is shown whole, since a decimal that changes every rep reads as noise"):
    val rate = Var(0.0)
    val shown = Readouts.perMinute(rate.signal)
    val seen = collection.mutable.ListBuffer.empty[String]
    val owner = new com.raquo.airstream.ownership.ManualOwner
    shown.foreach(seen += _)(using owner)

    // Zero is the starting value, which a signal reports on subscription; the rest are changes to it.
    Seq(29.4, 29.6, 30.0, 61.5).foreach(rate.set)
    owner.killSubscriptions()

    assertEquals(seen.toList, List("0", "29", "30", "30", "62"))
