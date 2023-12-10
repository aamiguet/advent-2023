package ch.aamiguet.advent2023

import scala.io.Source

object Day10 extends App:

  lazy val lines = Source.fromFile("data/day10.txt").getLines().toList

  enum Direction:
    case North
    case East
    case South
    case West
  import Direction.*

  case class Pipe(d1: Direction, d2: Direction):
    def nextDirection(from: Direction): Option[Direction] =
      val entry = from match
        case North => South
        case South => North
        case East => West
        case West => East
      if entry == d1 then Some(d2)
      else if entry == d2 then Some(d1)
      else None

  object Pipe:

    def apply(c: Char): Pipe = c match
      case '|' => Pipe(North, South)
      case '-' => Pipe(East, West)
      case 'L' => Pipe(North, East)
      case 'J' => Pipe(North, West)
      case '7' => Pipe(South, West)
      case 'F' => Pipe(South, East)

  case class Pos(x: Int, y: Int):
    def nextPosition(direction: Direction): Pos = direction match
      case North => Pos(x - 1, y)
      case East => Pos(x, y + 1)
      case South => Pos(x + 1, y)
      case West => Pos(x, y - 1)

  private def parse(lines: List[String]): Map[Pos, Pipe] =
    val pipes = for
      (l, i) <- lines.zipWithIndex
      (c, j) <- l.zipWithIndex if c != '.' && c != 'S'
    yield Pos(i, j) -> Pipe(c)
    pipes.toMap

  private def startingPos(lines: List[String]): Pos =
    val x = lines.indexWhere(_.contains('S'))
    val y = lines.drop(x).head.indexWhere(_ == 'S')
    Pos(x, y)

  private def findLoop(
      sp: Pos,
      pipes: Map[Pos, Pipe],
      direction: Direction,
      path: List[Pos]
  ): List[Pos] =
    val nextPos = path.head.nextPosition(direction)
    if nextPos == sp then nextPos :: path
    else
      val nextDirection =
        for
          pipe <- pipes.get(nextPos)
          nd <- pipe.nextDirection(direction)
        yield nd
      nextDirection match
        case Some(d) => findLoop(sp, pipes, d, nextPos :: path)
        case None => Nil

  private def findLoop(
      sp: Pos,
      pipes: Map[Pos, Pipe],
      directions: List[Direction] = List(North, East, South, West)
  ): List[Pos] =
    if directions.isEmpty then Nil
    else
      val loop = findLoop(sp, pipes, directions.head, List(sp))
      if loop.nonEmpty then loop
      else findLoop(sp, pipes, directions.tail)

  def part1(lines: List[String]): Int =
    val pipes = parse(lines)
    val sp = startingPos(lines)
    val loop = findLoop(sp, pipes)
    loop.size / 2

  println(part1(lines))
