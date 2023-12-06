package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day6.*

class Day6Test extends AnyFlatSpec with should.Matchers:

  val input =
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin

  val lines = input.split("\n").toList

  "Day6" should "solve both examples" in {
    assert(part1(lines) == 288L)
    assert(part2(lines) == 71503L)
  }
