package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day3.*

class Day3Test extends AnyFlatSpec with should.Matchers:

  val input =
    """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..""".stripMargin

  val lines = input.split("\n").toList

  "Day3" should "solve both examples" in {
    assert(part1(lines) == 4361)
    assert(part2(lines) == 467835)
  }
