package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day7.*

class Day7Test extends AnyFlatSpec with should.Matchers:

  val input =
    """32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483""".stripMargin

  val lines = input.split("\n").toList

  "Ordering" should "correctly order hands" in {
    val h1 = Hand("KK677", 1)
    val h2 = Hand("KTJJT", 1)
    assert(List(h1, h2).sorted == List(h2, h1))
  }

  "Day6" should "solve both examples" in {
    assert(part1(lines) == 6440)
    assert(part2(lines) == 5905)
  }
