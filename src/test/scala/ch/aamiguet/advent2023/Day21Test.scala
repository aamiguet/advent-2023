package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

class Day21Test extends AnyFlatSpec with should.Matchers:

  val d = Day21(
    """...........
      |.....###.#.
      |.###.##..#.
      |..#.#...#..
      |....#.#....
      |.##..S####.
      |.##..#...#.
      |.......##..
      |.##.#.####.
      |.##..##.##.
      |...........""".stripMargin.split("\n").toArray
  )

  "Day21" should "solve all example" in {
    assert(d.plotCount(6) == 16)
  }
