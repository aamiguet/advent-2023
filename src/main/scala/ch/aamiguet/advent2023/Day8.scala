package ch.aamiguet.advent2023

import scala.io.Source

object Day8 extends App:

  lazy val lines = Source.fromFile("data/day8.txt").getLines().toList

  val pattern = "\\w+".r

  case class Position(name: String)

  val startPos = Position("AAA")
  val endPos = Position("ZZZ")

  def parse(lines: List[String]): (Array[Char], Map[Position, (Position, Position)]) =
    val instructions = lines.head.toCharArray
    val m = lines
      .drop(2)
      .map: l =>
        val pos = pattern.findAllIn(l).toArray
        Position(pos(0)) -> (Position(pos(1)), Position(pos(2)))
    (instructions, m.toMap)

  def part1(lines: List[String]): Int =
    val (instructions, m) = parse(lines)
    var p = startPos
    var c = 0
    while p != endPos do
      instructions(c % instructions.size) match
        case 'L' => p = m(p)._1
        case 'R' => p = m(p)._2
      c += 1
    c

  def part2(lines: List[String]): Int =
    val (instructions, m) = parse(lines)
    var ps = m.keys.filter(_.name.endsWith("A")).toSet
    val endingPos = m.keys.filter(_.name.endsWith("Z")).toSet
    var c = 0
    while ps != endingPos do
      instructions(c % instructions.size) match
        case 'L' => ps = ps.map(m(_)._1)
        case 'R' => ps = ps.map(m(_)._2)
      println(ps)
      c += 1
    c

  println(part1(lines))
  //println(part2(lines))
