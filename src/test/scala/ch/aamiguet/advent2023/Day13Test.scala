package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day13.*

class Day13Test extends AnyFlatSpec with should.Matchers:

  val input =
    """#.##..##.
      |..#.##.#.
      |##......#
      |##......#
      |..#.##.#.
      |..##..##.
      |#.#.##.#.
      |
      |#...##..#
      |#....#..#
      |..##..###
      |#####.##.
      |#####.##.
      |..##..###
      |#....#..#""".stripMargin

  val lines = input.split("\n").toList

  val specialCase1 =
    """##..#.######..##.
      |.###..##...#.#..#
      |#.#..#..##.#..#.#
      |####.#..###..##..
      |......##.########
      |......##.###.####
      |####.#..###..##..
      |#.#..#..##.#..#.#
      |.###..##...#.#..#
      |##..#.######..##.
      |##..#.######..##.""".stripMargin.split("\n").toArray

  "Day13" should "solve both example" in {
    assert(part1(lines) == 405)
    assert(part2(lines) == 400)
  }

  "Day13" should "find the correct horizontal reflection" in {
    assert(Pattern(specialCase1).reflection == Horizontal(10))
  }
