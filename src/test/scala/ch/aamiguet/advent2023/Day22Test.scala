package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

class Day22Test extends AnyFlatSpec with should.Matchers:

  val d = Day22(
    """1,0,1~1,2,1
      |0,0,2~2,0,2
      |0,2,3~2,2,3
      |0,0,4~0,2,4
      |2,0,5~2,2,5
      |0,1,6~2,1,6
      |1,1,8~1,1,9""".stripMargin.split("\n").toList
  )

  "Day22" should "solve all example" in {
    assert(d.part1 == 5)
  }
