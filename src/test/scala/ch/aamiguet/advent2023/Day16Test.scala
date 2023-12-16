package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day16.*

class Day16Test extends AnyFlatSpec with should.Matchers:

  val lines =
    """.|...\....
      ||.-.\.....
      |.....|-...
      |........|.
      |..........
      |.........\
      |..../.\\..
      |.-.-/..|..
      |.|....-|.\
      |..//.|....""".stripMargin.split("\n").toList

  "Day16" should "solve both example" in {
    assert(part1(lines) == 46)
    assert(part2(lines) == 51)
  }
