package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day1.*

class Day1Test extends AnyFlatSpec with should.Matchers:

  val input1 =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin

  val input2 =
    """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen""".stripMargin

  val lines1 = input1.split("\n").toList
  val lines2 = input2.split("\n").toList

  "Day1" should "solve both examples" in {
    assert(part1(lines1) == 142)
    assert(part2(lines2) == 281)
  }
