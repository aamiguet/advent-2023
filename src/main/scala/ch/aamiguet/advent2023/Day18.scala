package ch.aamiguet.advent2023

import scala.io.Source

class Day18(lines: List[String]):

  case class Pos(x: Int, y: Int)

  case class Hole(color: String)

  case class Dig(direction: String, length: Int, color: String)

  case class Trench(from: Pos, to: Pos)

  object Dig:
    def apply(s: String): Dig =
      s.split(" ").toList match
        case d :: l :: c :: Nil => Dig(d, l.toInt, c.drop(2).dropRight(1))
        case _ => throw new Error("Not a dig")

  def digParameter(hex: String): (String, Int) =
    val d = hex.last match
      case '0' => "R"
      case '1' => "D"
      case '2' => "L"
      case '3' => "U"
    (d, Integer.parseInt(hex.drop(1).dropRight(1), 16))

  lazy val digs = lines.map(Dig(_))
  lazy val fixedDigs = lines.map: l =>
    val s = l.split(" ")(2).drop(1).dropRight(1)
    val (direction, length) = digParameter(s)
    Dig(direction, length, l)

  def trenchHoles(pos: Pos, dig: Dig): List[(Pos, Hole)] =
    val ps = dig.direction match
      case "R" => (pos.y + 1 to pos.y + dig.length).map(y => pos.copy(y = y))
      case "L" => (pos.y - 1 to pos.y - dig.length by -1).map(y => pos.copy(y = y))
      case "U" => (pos.x - 1 to pos.x - dig.length by -1).map(x => pos.copy(x = x))
      case "D" => (pos.x + 1 to pos.x + dig.length).map(x => pos.copy(x = x))
    ps.map((_, Hole(dig.color))).toList

  def trench(pos: Pos, dig: Dig): Trench =
    val to = dig.direction match
      case "R" => pos.copy(y = pos.y + dig.length)
      case "L" => pos.copy(y = pos.y - dig.length)
      case "U" => pos.copy(x = pos.x - dig.length)
      case "D" => pos.copy(x = pos.x + dig.length)
    Trench(pos, to)

  lazy val holes: Map[Pos, Hole] =
    digs
      .foldLeft((Pos(0, 0), Map.empty[Pos, Hole])): (acc, dig) =>
        val (p, hs) = acc
        val t = trenchHoles(p, dig)
        (t.last._1, (hs ++ t.toMap))
      ._2

  lazy val trenches: List[Trench] =
    fixedDigs
      .foldLeft((Pos(0, 0), List.empty[Trench])): (acc, dig) =>
        val (p, ts) = acc
        val t = trench(p, dig)
        (t.to, t :: ts)
      ._2

  object Part1:

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
          val next =
            toVisit.flatMap(neighbors).filterNot(p => allPos.contains(p) || out.contains(p))
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

    lazy val lavaArea = totalArea - outsideHoles.size

  object Part2:
    lazy val allVertexes = trenches.map(_.from)

    lazy val minX = allVertexes.map(_.x).min.toLong
    lazy val maxX = allVertexes.map(_.x).max.toLong
    lazy val minY = allVertexes.map(_.y).min.toLong
    lazy val maxY = allVertexes.map(_.y).max.toLong

    lazy val totalArea = (maxY - minY + 1L) * (maxX - minX + 1L)

    lazy val borderVertex =
      allVertexes.filter(p => p.x == minX || p.x == maxX || p.y == minY || p.y == maxY)

    lazy val outsideArea = 0L

    lazy val lavaArea =
      totalArea - outsideArea

  lazy val part1: Int = Part1.lavaArea
  lazy val part2: Long = Part2.lavaArea

object Day18 extends App:

  lazy val lines = Source.fromFile("data/day18.txt").getLines().toList

  val d = Day18(lines)

  println(d.part1)
  println(d.part2)
