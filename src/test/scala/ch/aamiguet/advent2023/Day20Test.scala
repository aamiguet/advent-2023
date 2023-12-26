package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

class Day20Test extends AnyFlatSpec with should.Matchers:

  val d1 = Day20(
    """broadcaster -> a, b, c
      |%a -> b
      |%b -> c
      |%c -> inv
      |&inv -> a""".stripMargin.split("\n").toList
  )

  val d2 = Day20(
    """broadcaster -> a
      |%a -> inv, con
      |&inv -> b
      |%b -> con
      |&con -> output""".stripMargin.split("\n").toList
  )

  "Day20" should "solve all example" in {
    assert(d1.part1 == 32000000L)
    assert(d2.part1 == 11687500L)
  }
