package ch.aamiguet.advent2023

import scala.io.Source

class Day22(lines: List[String]):

  case class Brick(
    xs: Range,
    ys: Range,
    zs: Range
  )

  object Brick:
    def apply(s: String): Brick =
      val s1 = s.split("~")
      val from = s1(0).split(",").map(_.toInt)
      val to = s1(1).split(",").map(_.toInt)
      Brick(
        from(0) to to(0),
        from(1) to to(1),
        from(2) to to(2)
      )

  lazy val bricks =
    lines.map(Brick(_))

  lazy val part1: Int = ???
  lazy val part2: Int = ???

object Day22 extends App:

  lazy val lines = Source.fromFile("data/day22.txt").getLines().toList

  val d = Day22(lines)
  println(d.part1)
