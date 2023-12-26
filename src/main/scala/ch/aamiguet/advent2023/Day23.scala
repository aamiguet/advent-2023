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

  private def nextPos(pos: Pos, visited: List[Pos]): List[Pos] =
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

  private def scenicPaths(
      current: List[List[Pos]] = List(List(startPos)),
      acc: List[List[Pos]] = List.empty
  )(nextPos: (Pos, List[Pos]) => List[Pos]): List[List[Pos]] =
    if current.isEmpty then acc
    else
      val (done, inProgress) = current.partition(_.head == endPos)
      val nextCurrent =
        inProgress.flatMap(ip => nextPos(ip.head, ip.tail).map(_ :: ip))
      scenicPaths(nextCurrent, acc ++ done)(nextPos)

  private val nextPossiblePosCache = scala.collection.mutable.Map.empty[Pos, List[Pos]]

  def nextPossiblePos(pos: Pos): List[Pos] =
    nextPossiblePosCache.getOrElseUpdate(
      pos, {
        List(
          pos.copy(x = pos.x + 1),
          pos.copy(x = pos.x - 1),
          pos.copy(y = pos.y + 1),
          pos.copy(y = pos.y - 1)
        ).filter(p => grid.isDefinedAt(p) && grid(p) != Forest)
      }
    )

  lazy val vertices: Set[Pos] =
    grid
      .filter: (pos, tile) =>
        tile != Forest && nextPossiblePos(pos).size > 2
      .map(_._1)
      .toSet + startPos + endPos

  private def findVertex(current: Pos, previous: Pos, distance: Int): (Pos, Int) =
    if vertices.contains(current) then (current, distance)
    else findVertex(nextPossiblePos(current).filter(_ != previous).head, current, distance + 1)

  lazy val verticesMap: Map[Pos, List[(Pos, Int)]] =
    vertices
      .map: v =>
        v -> nextPossiblePos(v).map(findVertex(_, v, 1))
      .toMap

  private def longestPathIgnoringSlope(
      current: List[(List[Pos], Int)] = List((List(startPos), 0)),
      longest: (List[Pos], Int) = (List.empty, 0)
  ): (List[Pos], Int) =
    if current.isEmpty then longest
    else
      val (done, inProgress) = current.partition(_._1.head == endPos)
      val newLongest =
        done.foldLeft(longest): (acc, d) =>
          if d._2 > acc._2 then d
          else acc
      val nextCurrent =
        inProgress.flatMap: p =>
          verticesMap(p._1.head)
            .filterNot(n => p._1.contains(n._1))
            .map(n => (n._1 :: p._1, n._2 + p._2))
      longestPathIgnoringSlope(nextCurrent, newLongest)

  lazy val part1: Int =
    scenicPaths()(nextPos).map(_.size).max - 1
  lazy val part2: Int =
    longestPathIgnoringSlope()._2

object Day23 extends App:

  lazy val lines = Source.fromFile("data/day23.txt").getLines().toArray

  val d = Day23(lines)
  println(d.part1)
  println(d.part2)
