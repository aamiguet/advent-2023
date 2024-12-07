package ch.aamiguet.advent2023

import scala.io.Source

object Day8 extends App:

  type Network = Map[Position, (Position, Position)]

  lazy val lines = Source.fromFile("data/day8.txt").getLines().toList

  val pattern = "\\w+".r

  case class Position(name: String)

  val startPos = Position("AAA")
  val endPos = Position("ZZZ")

  def parse(lines: List[String]): (Array[Char], Network) =
    val instructions = lines.head.toCharArray
    val m = lines
      .drop(2)
      .map: l =>
        val pos = pattern.findAllIn(l).toArray
        Position(pos(0)) -> (Position(pos(1)), Position(pos(2)))
    (instructions, m.toMap)

  def nextPos(pos: Position, stepCount: Int)(using instructions: Array[Char], network: Network): Position =
    instructions(stepCount % instructions.size) match
      case 'L' => network(pos)._1
      case 'R' => network(pos)._2

  def exit(startPos: Position, endPos: Set[Position])(using instructions: Array[Char], network: Network): (Int, Position) =
    def loop(pos: Position, stepCount: Int): (Int, Position) =
      if endPos(pos) then (stepCount, pos)
      else loop(nextPos(pos, stepCount), stepCount + 1)
    loop(nextPos(startPos, 0), 1)

  def part1(lines: List[String]): Int =
    val (instructions, network) = parse(lines)
    exit(startPos, Set(endPos))(using instructions, network)._1

  def startLoopStep(initialPos: Position, initialStepCount: Int)(using instructions: Array[Char], network: Network): Int =
    def loop(pos: Position, stepCount: Int): Int =
      if pos == initialPos then stepCount
      else loop(nextPos(pos, stepCount), stepCount + 1)
    loop(nextPos(initialPos, initialStepCount), initialStepCount + 1)

  def gcd(x: Long, y: Long): Long =
    def gcd0(x: Long, y: Long): Long =
      val rest = x % y
      if rest == 0 then y
      else gcd(y, rest)
    if x >= y then gcd0(x, y)
    else gcd0(y, x)

  def lcm(x: Long, y: Long): Long =
    (x * y)/gcd(x, y)

  def lcm(loopSteps: Array[Long]): Long =
    loopSteps.fold(1L)(lcm)

  def part2(lines: List[String]): Long =
    val (instructions, m) = parse(lines)
    val ps = m.keys.filter(_.name.endsWith("A")).toArray
    val endingPos = m.keys.filter(_.name.endsWith("Z")).toSet
    val loopSteps = ps.map: p =>
      // for some reason the stepcount to the first exit is the loop time (after that we keep getting to an exit at the same frequency)
      val (loopCount, endPos) = exit(p, endingPos)(using instructions, m)
      loopCount.toLong
    lcm(loopSteps)

  println(part1(lines))
  println(part2(lines))
