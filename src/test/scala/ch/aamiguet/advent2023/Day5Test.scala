package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day5.*

class Day5Test extends AnyFlatSpec with should.Matchers:

  val input =
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4""".stripMargin

  val lines = input.split("\n").toList

  "Rule" should "compute location correctly" in {
    assert(Rule(52, 50, 48).destination(79) == Some(81L))
    assert(Rule(52, 50, 48).destination(14) == None)
  }

  "Day5" should "solve both examples" in {
    assert(part1(lines) == 35L)
    assert(part2(lines) == 46L)
  }
