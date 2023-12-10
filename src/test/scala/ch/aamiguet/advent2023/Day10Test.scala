package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day10.*

class Day10Test extends AnyFlatSpec with should.Matchers:

  val input1 =
    """-L|F7
      |7S-7|
      |L|7||
      |-L-J|
      |L|-JF""".stripMargin
  val input2 =
    """7-F7-
      |.FJ|7
      |SJLL7
      ||F--J
      |LJ.LJ""".stripMargin

  val lines1 = input1.split("\n").toList
  val lines2 = input2.split("\n").toList

  "Day10" should "solve all examples" in {
    assert(part1(lines1) == 4)
    assert(part1(lines2) == 8)
  }
