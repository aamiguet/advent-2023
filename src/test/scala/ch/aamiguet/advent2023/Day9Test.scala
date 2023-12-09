package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day9.*

class Day9Test extends AnyFlatSpec with should.Matchers:

  val input =
    """0 3 6 9 12 15
      |1 3 6 10 15 21
      |10 13 16 21 30 45""".stripMargin

  val lines = input.split("\n").toList

  "Day9" should "solve both examples" in {
    assert(part1(lines) == 114)
    assert(part2(lines) == 2)
  }
