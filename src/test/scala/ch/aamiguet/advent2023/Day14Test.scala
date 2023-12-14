package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day14.*

class Day14Test extends AnyFlatSpec with should.Matchers:

  val input =
    """O....#....
      |O.OO#....#
      |.....##...
      |OO.#O....O
      |.O.....O#.
      |O.#..O.#.#
      |..O..#O..O
      |.......O..
      |#....###..
      |#OO..#....""".stripMargin

  val lines = input.split("\n").toArray

  "Day14" should "compute load correctly" in {
    val tilted =
      """OOOO.#.O..
        |OO..#....#
        |OO..O##..O
        |O..#.OO...
        |........#.
        |..#....#.#
        |..O..#.O.O
        |..O.......
        |#....###..
        |#....#....""".stripMargin.split("\n").toArray
    assert(load(parse(tilted)) == 136)
  }

  "Day14" should "solve both example" in {
    assert(part1(lines) == 136)
    assert(part2(lines) == 64)
  }
