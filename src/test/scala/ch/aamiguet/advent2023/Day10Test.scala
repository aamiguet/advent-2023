package ch.aamiguet.advent2023

import org.scalatest.*
import flatspec.*
import matchers.*

import Day10.*

class Day10Test extends AnyFlatSpec with should.Matchers:

  val input1 =
    """-L|F7
      |7S-7|
      |L|7||
      |-L-J|
      |L|-JF""".stripMargin
  val input2 =
    """7-F7-
      |.FJ|7
      |SJLL7
      ||F--J
      |LJ.LJ""".stripMargin

  val lines1 = input1.split("\n").toList
  val lines2 = input2.split("\n").toList

  "Day10" should "solve both examples for part 1" in {
    assert(part1(lines1) == 4)
    assert(part1(lines2) == 8)
  }

  "Day10" should "solve first example for part 2" in {
    val lines =
      """...........
        |.S-------7.
        |.|F-----7|.
        |.||.....||.
        |.||.....||.
        |.|L-7.F-J|.
        |.|..|.|..|.
        |.L--J.L--J.
        |...........""".stripMargin.split("\n").toList
    assert(part2(lines) == 4)
  }

  "Day10" should "solve first example variation for part 2" in {
    val lines =
      """..........
        |.S------7.
        |.|F----7|.
        |.||....||.
        |.||....||.
        |.|L-7F-J|.
        |.|..||..|.
        |.L--JL--J.
        |..........""".stripMargin.split("\n").toList
    assert(part2(lines) == 4)
  }

  "Day10" should "solve large example for part 2" in {
    val lines =
      """.F----7F7F7F7F-7....
        |.|F--7||||||||FJ....
        |.||.FJ||||||||L7....
        |FJL7L7LJLJ||LJ.L-7..
        |L--J.L7...LJS7F-7L7.
        |....F-J..F7FJ|L7L7L7
        |....L7.F7||L7|.L7L7|
        |.....|FJLJ|FJ|F7|.LJ
        |....FJL-7.||.||||...
        |....L---J.LJ.LJLJ...""".stripMargin.split("\n").toList
    assert(part2(lines) == 8)
  }

  "Day10" should "solve large example with junk for part 2" in {
    val lines =
      """FF7FSF7F7F7F7F7F---7
        |L|LJ||||||||||||F--J
        |FL-7LJLJ||||||LJL-77
        |F--JF--7||LJLJ7F7FJ-
        |L---JF-JLJ.||-FJLJJ7
        ||F|F-JF---7F7-L7L|7|
        ||FFJF7L7F-JF7|JL---7
        |7-L-JL7||F7|L7F-7F7|
        |L.L7LFJ|||||FJL7||LJ
        |L7JLJL-JLJLJL--JLJ.L""".stripMargin.split("\n").toList
    assert(part2(lines) == 10)
  }
