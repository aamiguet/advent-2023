package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

class Day17Test extends AnyFlatSpec with should.Matchers:

  val d = Day17(
    """2413432311323
      |3215453535623
      |3255245654254
      |3446585845452
      |4546657867536
      |1438598798454
      |4457876987766
      |3637877979653
      |4654967986887
      |4564679986453
      |1224686865563
      |2546548887735
      |4322674655533""".stripMargin.split("\n").toList
  )

  "Day17" should "solve both example" in {
    assert(d.part1 == 102)
    // assert(d.part2 == 102)
  }
