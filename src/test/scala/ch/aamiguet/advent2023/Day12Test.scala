package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day12.*

class Day12Test extends AnyFlatSpec with should.Matchers:

  val input =
    """???.### 1,1,3
      |.??..??...?##. 1,1,3
      |?#?#?#?#?#?#?#? 1,3,1,6
      |????.#...#... 4,1,1
      |????.######..#####. 1,6,5
      |?###???????? 3,2,1""".stripMargin

  val lines = input.split("\n").toList

  "Day12" should "solve both example" in {
    assert(part1(lines) == 21L)
    assert(part2(lines) == 525152L)
  }
