package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

class Day18Test extends AnyFlatSpec with should.Matchers:

  val d = Day18(
    """R 6 (#70c710)
      |D 5 (#0dc571)
      |L 2 (#5713f0)
      |D 2 (#d2c081)
      |R 2 (#59c680)
      |D 2 (#411b91)
      |L 5 (#8ceee2)
      |U 2 (#caa173)
      |L 1 (#1b58a2)
      |U 2 (#caa171)
      |R 2 (#7807d2)
      |U 3 (#a77fa3)
      |L 2 (#015232)
      |U 2 (#7a21e3)""".stripMargin.split("\n").toList
  )

  "Day18" should "correctly convert hex to direction & distance" in {
    assert(d.digParameter("#70c710") == ("R", 461937))
    assert(d.digParameter("#a77fa3") == ("U", 686074))
    assert(d.digParameter("#411b91") == ("D", 266681))
  }

  "Day18" should "solve both example" in {
    assert(d.part1 == 62)
    assert(d.part2 == 952408144115L)
  }
