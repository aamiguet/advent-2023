package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day2.*

class Day2Test extends AnyFlatSpec with should.Matchers:

  val input =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin

  val lines = input.split("\n").toList

  "CubeSet" should "parse correctly" in {
    assert(CubeSet("3 blue, 4 red") == CubeSet(4, 0, 3))
    assert(CubeSet("8 green, 6 blue, 20 red") == CubeSet(20, 8, 6))
  }

  "Game" should "parse correctly" in {
    val game = Game("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")
    val expected = Game(3, List(CubeSet(20, 8, 6), CubeSet(4, 13, 5), CubeSet(1, 5, 0)))
    assert(game == expected)
  }

  "Day2" should "solve both examples" in {
    assert(part1(lines) == 8)
    assert(part2(lines) == 2286)
  }
