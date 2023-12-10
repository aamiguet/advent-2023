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

  private def findLoop0(
      start: (Pos, Direction),
      pipes: Map[Pos, Pipe],
      path: List[(Pos, Direction)]
  ): List[(Pos, Direction)] =
    val (pos, direction) = path.head
    val nextPos = pos.nextPosition(direction)
    if nextPos == start._1 then start :: path
    else
      val nextDirection =
        for
          pipe <- pipes.get(nextPos)
          nd <- pipe.nextDirection(direction)
        yield nd
      nextDirection match
        case Some(d) => findLoop0(start, pipes, (nextPos, d) :: path)
        case None => Nil

  private def findLoop(
      sp: Pos,
      pipes: Map[Pos, Pipe],
      directions: List[Direction] = List(North, East, South, West)
  ): List[(Pos, Direction)] =
    if directions.isEmpty then Nil
    else
      val start = (sp, directions.head)
      val loop = findLoop0(start, pipes, List(start))
      if loop.nonEmpty then loop
      else findLoop(sp, pipes, directions.tail)

  def part1(lines: List[String]): Int =
    val pipes = parse(lines)
    val sp = startingPos(lines)
    val loop = findLoop(sp, pipes)
    loop.size / 2

  private def leftDirections(entry: Direction, exit: Direction): List[Direction] =
    (entry, exit) match
      case (South, North) => List(West)
      case (North, South) => List(East)
      case (East, West) => List(South)
      case (West, East) => List(North)
      case (South, East) => List(West, North)
      case (East, North) => List(South, West)
      case (North, West) => List(East, South)
      case (West, South) => List(North, East)
      case _ => Nil

  private def rightDirections(entry: Direction, exit: Direction): List[Direction] =
    (entry, exit) match
      case (South, North) => List(East)
      case (North, South) => List(West)
      case (East, West) => List(North)
      case (West, East) => List(South)
      case (South, West) => List(East, North)
      case (West, North) => List(South, East)
      case (North, East) => List(West, South)
      case (East, South) => List(North, West)
      case _ => Nil

  private def switchDirection(direction: Direction) =
    direction match
      case North => South
      case West => East
      case South => North
      case East => West

  def part2(lines: List[String]): Int =
    val allDirections = List(North, South, East, West)
    val pipes = parse(lines)
    val sp = startingPos(lines)
    val loop = findLoop(sp, pipes)
    val pathTiles = loop.map(_._1).toSet
    val maxX = lines.size
    val maxY = lines.head.size
    val loopWithEntry = loop.zip(loop.tail).map((p1, p2) => (p1._1, switchDirection(p2._2), p1._2))
    val (leftTiles, rightTiles) = loopWithEntry.foldLeft((Set.empty[Pos], Set.empty[Pos])):
      (acc, b) =>
        def addConnectedTiles0(pos: List[Pos], tiles: Set[Pos]): Set[Pos] =
          if pos.isEmpty then tiles
          else
            val p = pos.head
            val connectedPos =
              allDirections.map(p.nextPosition(_))
            val newPos = (pos.tail ++ connectedPos).filter(p =>
              p.x >= 0 && p.x <= maxX && p.y >= 0 && p.y <= maxY && !tiles.contains(p) && !pathTiles
                .contains(p)
            )
            addConnectedTiles0(newPos, tiles + p)

        def addConnectedTiles(pos: Pos, directions: List[Direction], tiles: Set[Pos]): Set[Pos] =
          val cands = directions
            .map(pos.nextPosition(_))
            .filter(p =>
              p.x >= 0 && p.x <= maxX && p.y >= 0 && p.y <= maxY && !tiles.contains(p) && !pathTiles
                .contains(p)
            )
          addConnectedTiles0(cands, tiles)

        val (pos, entry, exit) = b
        val (leftTiles, rightTiles) = acc
        val lds = leftDirections(entry, exit)
        val rds = rightDirections(entry, exit)
        (addConnectedTiles(pos, lds, leftTiles), addConnectedTiles(pos, rds, rightTiles))

    // assuming (0, 0) is not enclosed (and not on the path)
    println(pathTiles.contains(Pos(0, 0)))
    if leftTiles.contains(Pos(0, 0)) then rightTiles.size
    else leftTiles.size

  println(part1(lines))
  println(part2(lines))
