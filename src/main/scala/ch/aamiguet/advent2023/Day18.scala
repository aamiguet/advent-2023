package ch.aamiguet.advent2023

import scala.io.Source

class Day18(lines: List[String]):

  case class Pos(x: Int, y: Int)

  case class Hole(color: String)

  case class Dig(direction: String, length: Int, color: String)

  object Dig:
    def apply(s: String): Dig =
      s.split(" ").toList match
        case d :: l :: c :: Nil => Dig(d, l.toInt, c.drop(2).dropRight(1))
        case _ => throw new Error("Not a dig")

  lazy val digs = lines.map(Dig(_))

  def trench(pos: Pos, dig: Dig): List[(Pos, Hole)] =
    val ps = dig.direction match
      case "R" => (pos.y + 1 to pos.y + dig.length).map(y => pos.copy(y = y))
      case "L" => (pos.y - 1 to pos.y - dig.length by -1).map(y => pos.copy(y = y))
      case "U" => (pos.x - 1 to pos.x - dig.length by -1).map(x => pos.copy(x = x))
      case "D" => (pos.x + 1 to pos.x + dig.length).map(x => pos.copy(x = x))
    ps.map((_, Hole(dig.color))).toList

  lazy val holes: Map[Pos, Hole] =
    digs
      .foldLeft((Pos(0, 0), Map.empty[Pos, Hole])): (acc, dig) =>
        val (p, hs) = acc
        val t = trench(p, dig)
        (t.last._1, (hs ++ t.toMap))
      ._2
  lazy val allPos = holes.keys.toSet

  lazy val minX = allPos.map(_.x).min
  lazy val maxX = allPos.map(_.x).max
  lazy val minY = allPos.map(_.y).min
  lazy val maxY = allPos.map(_.y).max

  lazy val totalArea = (maxY - minY + 1) * (maxX - minX + 1)

  def neighbors(pos: Pos): List[Pos] =
    List(
      pos.copy(x = pos.x - 1),
      pos.copy(x = pos.x + 1),
      pos.copy(y = pos.y + 1),
      pos.copy(y = pos.y - 1)
    ).filter(p => p.x >= minX && p.x <= maxX && p.y >= minY && p.y <= maxY)

  lazy val outsideHoles =
    def loop(toVisit: Set[Pos], outside: Set[Pos] = Set.empty): Set[Pos] =
      if toVisit.isEmpty then outside
      else
        val out = outside ++ toVisit
        val next = toVisit.flatMap(neighbors).filterNot(p => allPos.contains(p) || out.contains(p))
        loop(next, out)

    val border =
      (
        (for
          x <- (minX to maxX).toList
          y <- List(minY, maxY)
        yield Pos(x, y)) ++
        (for
          y <- (minY to maxY).toList
          x <- List(minX, maxX)
        yield Pos(x, y))
      ).filterNot(allPos.contains)
    loop(border.toSet)

  lazy val part1: Int = totalArea - outsideHoles.size
  lazy val part2: Int = ???

object Day18 extends App:

  lazy val lines = Source.fromFile("data/day18.txt").getLines().toList

  val d = Day18(lines)

  println(d.part1)
  println(d.part2)
