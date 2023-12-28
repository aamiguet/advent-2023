package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

class Day24Test extends AnyFlatSpec with should.Matchers:

  val d = Day24(
    """19, 13, 30 @ -2, 1, -2
      |18, 19, 22 @ -1, -1, -2
      |20, 25, 34 @ -2, -2, -4
      |12, 31, 28 @ -1, -2, -1
      |20, 19, 15 @ 1, -5, -3""".stripMargin.split("\n").toList
  )

  "Day24" should "solve all example" in {
    assert(d.crossingFuturePathCount(7d, 27d) == 2)
    assert(d.part2 == 47)
  }
