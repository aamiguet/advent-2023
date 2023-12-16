package ch.aamiguet.advent2023

import scala.io.Source

object Day16 extends App:

  lazy val lines = Source.fromFile("data/day16.txt").getLines().toList

  case class Pos(x: Int, y: Int)

  enum Tile:
    case Empty extends Tile
    case FrontMirror extends Tile
    case BackMirror extends Tile
    case VerticalSplitter extends Tile
    case HorizontalSplitter extends Tile
  import Tile.*

  enum Direction:
    case Rightward extends Direction
    case Leftward extends Direction
    case Downward extends Direction
    case Upward extends Direction
  import Direction.*

  object Tile:
    def apply(c: Char): Tile = c match
      case '.' => Empty
      case '/' => FrontMirror
      case '\\' => BackMirror
      case '|' => VerticalSplitter
      case '-' => HorizontalSplitter

  private def parseGrid(lines: List[String]): Map[Pos, Tile] =
    (for
      i <- 0 until lines.size
      j <- 0 until lines.head.size
    yield Pos(i, j) -> Tile(lines(i)(j))).toMap

  private def nextToVisit(
      pos: Pos,
      direction: Direction,
      grid: Map[Pos, Tile]
  ): List[(Pos, Direction)] =
    val cands = grid(pos) match
      case VerticalSplitter if direction == Rightward || direction == Leftward =>
        List((pos.copy(x = pos.x + 1), Downward), (pos.copy(x = pos.x - 1), Upward))
      case HorizontalSplitter if direction == Upward || direction == Downward =>
        List((pos.copy(y = pos.y + 1), Rightward), (pos.copy(y = pos.y - 1), Leftward))
      case FrontMirror =>
        direction match
          case Rightward => List((pos.copy(x = pos.x - 1), Upward))
          case Leftward => List((pos.copy(x = pos.x + 1), Downward))
          case Upward => List((pos.copy(y = pos.y + 1), Rightward))
          case Downward => List((pos.copy(y = pos.y - 1), Leftward))
      case BackMirror =>
        direction match
          case Rightward => List((pos.copy(x = pos.x + 1), Downward))
          case Leftward => List((pos.copy(x = pos.x - 1), Upward))
          case Upward => List((pos.copy(y = pos.y - 1), Leftward))
          case Downward => List((pos.copy(y = pos.y + 1), Rightward))
      case _ =>
        direction match
          case Rightward => List((pos.copy(y = pos.y + 1), Rightward))
          case Leftward => List((pos.copy(y = pos.y - 1), Leftward))
          case Upward => List((pos.copy(x = pos.x - 1), Upward))
          case Downward => List((pos.copy(x = pos.x + 1), Downward))
    cands.filter((p, _) => grid.contains(p))

  private def litPos(
      grid: Map[Pos, Tile],
      toVisit: List[(Pos, Direction)],
      visited: Set[(Pos, Direction)],
      lit: Set[Pos]
  ): Set[Pos] =
    if toVisit.isEmpty then lit
    else
      val (p, d) = toVisit.head
      val next = nextToVisit(p, d, grid).filterNot(visited.contains)
      litPos(grid, toVisit.tail ++ next, visited + toVisit.head, lit + p)

  private def energy(grid: Map[Pos, Tile], init: (Pos, Direction)): Int =
    val lit = litPos(grid, List(init), Set.empty[(Pos, Direction)], Set.empty[Pos])
    lit.size

  def part1(lines: List[String]): Int =
    val grid = parseGrid(lines)
    val topLeft = (Pos(0, 0), Rightward)
    energy(grid, topLeft)

  def part2(lines: List[String]) =
    val grid = parseGrid(lines)
    val xSize = lines.size
    val ySize = lines.head.size
    val inits =
      (0 until ySize).map(y => (Pos(0, y), Downward)) ++
        (0 until xSize).map(x => (Pos(x, 0), Rightward)) ++
        (0 until ySize).map(y => (Pos(xSize - 1, y), Upward)) ++
        (0 until xSize).map(x => (Pos(x, ySize - 1), Leftward))
    inits.map(energy(grid, _)).max

  println(part1(lines))
  println(part2(lines))
