package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day11.*

class Day11Test extends AnyFlatSpec with should.Matchers:

  val input =
    """...#......
      |.......#..
      |#.........
      |..........
      |......#...
      |.#........
      |.........#
      |..........
      |.......#..
      |#...#.....""".stripMargin

  val lines = input.split("\n").toList

  "Day11" should "solve part 1 example" in {
    assert(part1(lines) == 374L)
  }

  "Day11" should "solve both part 2 examples" in {
    assert(part(lines, 10L) == 1030L)
    assert(part(lines, 100L) == 8410L)
  }
