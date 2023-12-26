package ch.aamiguet.advent2023

import scala.io.Source

class Day23(lines: Array[String]):

  enum Tile:
    case Forest, Path, RightSlope, LeftSlope, UpSlope, DownSlope
  import Tile.*

  case class Pos(x: Int, y: Int)

  lazy val xSize = lines.size
  lazy val ySize = lines.head.size

  lazy val grid =
    val g = for
      i <- 0 until xSize
      j <- 0 until ySize
      t = lines(i)(j) match
        case '#' => Forest
        case '>' => RightSlope
        case '<' => LeftSlope
        case '^' => UpSlope
        case 'v' => DownSlope
        case _ => Path
    yield Pos(i, j) -> t
    g.toMap

  lazy val startPos = Pos(0, 1)
  lazy val endPos = Pos(xSize - 1, ySize - 2)

  def nextPos(pos: Pos, visited: List[Pos]): List[Pos] =
    val cands =
      grid(pos) match
        case RightSlope => List(pos.copy(y = pos.y + 1))
        case LeftSlope => List(pos.copy(y = pos.y - 1))
        case UpSlope => List(pos.copy(x = pos.x - 1))
        case DownSlope => List(pos.copy(x = pos.x + 1))
        case _ =>
          List(
            pos.copy(x = pos.x + 1),
            pos.copy(x = pos.x - 1),
            pos.copy(y = pos.y + 1),
            pos.copy(y = pos.y - 1)
          )
    cands.filter(p => grid.isDefinedAt(p) && grid(p) != Forest && !visited.contains(p))

  def nextPosIgnoringSlope(pos: Pos, visited: Set[Pos]): List[Pos] =
    val cands =
      List(
        pos.copy(x = pos.x + 1),
        pos.copy(x = pos.x - 1),
        pos.copy(y = pos.y + 1),
        pos.copy(y = pos.y - 1)
      )
    cands.filter(p => grid.isDefinedAt(p) && grid(p) != Forest && !visited.contains(p))

  def scenicPaths(
      current: List[List[Pos]] = List(List(startPos)),
      acc: List[List[Pos]] = List.empty
  )(nextPos: (Pos, List[Pos]) => List[Pos]): List[List[Pos]] =
    if current.isEmpty then acc
    else
      val (done, inProgress) = current.partition(_.head == endPos)
      val nextCurrent =
        inProgress.flatMap(ip => nextPos(ip.head, ip.tail).map(_ :: ip))
      scenicPaths(nextCurrent, acc ++ done)(nextPos)

  def scenicPathLengths(
      current: Set[(Pos, Set[Pos])] = Set((startPos, Set.empty)),
      acc: Set[Int] = Set.empty
  )(nextPos: (Pos, Set[Pos]) => List[Pos]): Set[Int] =
    if current.isEmpty then acc
    else
      val (done, inProgress) = current.partition(_._1 == endPos)
      val nextCurrent =
        inProgress.flatMap: ip =>
          val nextVisited = ip._2 + ip._1
          nextPos(ip._1, ip._2).map(p => (p, nextVisited))

      scenicPathLengths(nextCurrent, acc ++ done.map(_._2.size))(nextPos)

  lazy val part1: Int =
    scenicPaths()(nextPos).map(_.size).max - 1
  lazy val part2: Int =
    scenicPathLengths()(nextPosIgnoringSlope).max

object Day23 extends App:

  lazy val lines = Source.fromFile("data/day23.txt").getLines().toArray

  val d = Day23(lines)
  println(d.part1)
  // println(d.part2)
