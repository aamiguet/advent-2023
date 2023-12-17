package ch.aamiguet.advent2023

import scala.io.Source

class Day17(lines: List[String]):

  case class Pos(x: Int, y: Int)

  private lazy val cityGrid: Map[Pos, Int] =
    (for
      (l, i) <- lines.zipWithIndex
      j <- 0 until l.size
    yield Pos(i, j) -> l(j).toString.toInt).toMap

  private lazy val xSize = lines.size
  private lazy val ySize = lines.head.size

  private lazy val visitMap = scala.collection.mutable.Map.empty[Pos, List[Visit]]

  enum Direction:
    case Left extends Direction
    case Top extends Direction
    case Right extends Direction
    case Bottom extends Direction
  import Direction.*

  private def oppositeDirection(direction: Direction): Direction = direction match
    case Left => Right
    case Top => Bottom
    case Right => Left
    case Bottom => Top

  case class Visit(
      pos: Pos,
      from: Direction,
      blockCount: Int,
      cost: Int,
      path: List[Pos]
  ):
    lazy val oppositeFrom = oppositeDirection(from)

    def isBetter(other: Visit): Boolean =
      from match
        case other.from => blockCount <= other.blockCount && cost <= other.cost
        case _ => (blockCount < other.blockCount && cost <= other.cost)

    def isComparable(other: Visit): Boolean =
      (from != other.oppositeFrom) &&
        (from == other.from || (from != other.from && blockCount <= other.blockCount))

    def hasToBeVisited(visits: List[Visit]): Boolean =
      visits.filter(this.isComparable(_)) match
        case Nil => true
        case vs => !vs.exists(_.isBetter(this))

  private def minCost(pos: Pos): Int =
    val f = visitMap(pos).sortBy(_.cost)
    f.head.cost + cityGrid(pos)

  private def removeWorse(visits: List[Visit], visit: Visit): List[Visit] =
    visit :: visits.filterNot(v => visit.isComparable(v) && visit.isBetter(v))

  private def hasToBeVisited(visit: Visit): Boolean =
    visitMap.get(visit.pos) match
      case Some(vs) => visit.hasToBeVisited(vs)
      case None => true

  private def isInbound(pos: Pos): Boolean =
    pos.x >= 0 && pos.x < xSize && pos.y >= 0 && pos.y < ySize

  private def updateVisits(visit: Visit) =
    visitMap.updateWith(visit.pos):
      case Some(vs) =>
        Some(removeWorse(vs, visit))
      case None => Some(List(visit))

  private def nextForwardPos(
      pos: Pos,
      direction: Direction
  ): List[(Pos, Direction)] =
    val nextPos = direction match
      case Left => pos.copy(y = pos.y + 1)
      case Right => pos.copy(y = pos.y - 1)
      case Top => pos.copy(x = pos.x + 1)
      case Bottom => pos.copy(x = pos.x - 1)
    List((nextPos, direction)).filter((p, _) => isInbound(p))

  private def nextTurnPos(
      pos: Pos,
      direction: Direction
  ): List[(Pos, Direction)] =
    val n =
      if direction == Left || direction == Right then
        List((pos.copy(x = pos.x + 1), Top), (pos.copy(x = pos.x - 1), Bottom))
      else List((pos.copy(y = pos.y + 1), Left), (pos.copy(y = pos.y - 1), Right))
    n.filter((p, _) => isInbound(p))

  private def nextVisit(
      visit: Visit
  ): List[Visit] =
    updateVisits(visit)
    val incrementedCost = visit.cost + cityGrid(visit.pos)
    val extendedPath = visit.pos :: visit.path
    val forwardCands =
      if visit.blockCount < 3 then
        nextForwardPos(visit.pos, visit.from).map((p, d) =>
          Visit(p, d, visit.blockCount + 1, incrementedCost, extendedPath)
        )
      else Nil
    val turnCands = nextTurnPos(visit.pos, visit.from).map((p, d) =>
      Visit(p, d, 1, incrementedCost, extendedPath)
    )
    val cands = forwardCands ++ turnCands
    cands

  private def computeCost(toVisit: List[Visit]): Unit =
    val filteredToVisit = toVisit.filter(hasToBeVisited(_)).sortBy(_.cost)
    if filteredToVisit.nonEmpty then
      nextVisit(filteredToVisit.head) match
        case Nil => computeCost(filteredToVisit.tail)
        case vs => computeCost(filteredToVisit.tail ++ vs)

  lazy val part1: Int =
    val initPath = List(Pos(0, 0))
    val toVisit = List(
      Visit(
        Pos(0, 1),
        Left,
        1,
        0,
        initPath
      ),
      Visit(
        Pos(1, 0),
        Top,
        1,
        0,
        initPath
      )
    )
    computeCost(toVisit)
    minCost(Pos(xSize - 1, ySize - 1))

  lazy val part2: Int = ???

object Day17 extends App:

  lazy val lines = Source.fromFile("data/day17.txt").getLines().toList

  val d = Day17(lines)

  println(d.part1)
  println(d.part2)
