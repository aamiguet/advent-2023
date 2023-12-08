package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day8.*

class Day8Test extends AnyFlatSpec with should.Matchers:

  val input1 =
    """RL
      |
      |AAA = (BBB, CCC)
      |BBB = (DDD, EEE)
      |CCC = (ZZZ, GGG)
      |DDD = (DDD, DDD)
      |EEE = (EEE, EEE)
      |GGG = (GGG, GGG)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin

  val input2 =
    """LLR
      |
      |AAA = (BBB, BBB)
      |BBB = (AAA, ZZZ)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin

  val input3 =
    """LR
      |
      |11A = (11B, XXX)
      |11B = (XXX, 11Z)
      |11Z = (11B, XXX)
      |22A = (22B, XXX)
      |22B = (22C, 22C)
      |22C = (22Z, 22Z)
      |22Z = (22B, 22B)
      |XXX = (XXX, XXX)""".stripMargin

  val lines1 = input1.split("\n").toList
  val lines2 = input2.split("\n").toList
  val lines3 = input3.split("\n").toList

  "Day8" should "solve all examples" in {
    assert(part1(lines1) == 2)
    assert(part1(lines2) == 6)
    assert(part2(lines3) == 6)
  }
