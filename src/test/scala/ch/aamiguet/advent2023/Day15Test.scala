package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day15.*

class Day15Test extends AnyFlatSpec with should.Matchers:

  val line = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

  "Day15" should "compute hash correctly" in {
    assert(hash("rn=1") == 30)
  }

  "Day15" should "solve both example" in {
    assert(part1(line) == 1320)
    assert(part2(line) == 145)
  }
