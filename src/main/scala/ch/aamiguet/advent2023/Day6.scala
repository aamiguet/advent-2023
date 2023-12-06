package ch.aamiguet.advent2023

import scala.io.Source

object Day6 extends App:

  lazy val lines = Source.fromFile("data/day6.txt").getLines().toList
  val pattern = "\\d+".r

  case class Race(
      time: Long,
      record: Long
  ):
    lazy val waysToBeat: Long =
      val b = -time.toDouble
      val c = record.toDouble
      val root = math.sqrt(math.pow(b, 2d) - 4d * c)
      val highSolution = (-b + root) / 2d
      val lowSolution = (-b - root) / 2d
      val upperBound = if highSolution.isWhole then highSolution - 1d else math.floor(highSolution)
      val lowerBound = if lowSolution.isWhole then lowSolution + 1d else math.ceil(lowSolution)
      (upperBound - lowerBound + 1d).toLong

  private def parse(lines: List[String]): List[Race] =
    val times = pattern.findAllIn(lines.head).map(_.toLong).toList
    val records = pattern.findAllIn(lines(1)).map(_.toLong).toList

    times.zip(records).map(Race(_, _))

  private def parseBigRace(lines: List[String]): Race =
    val time = pattern.findAllIn(lines.head).mkString.toLong
    val record = pattern.findAllIn(lines(1)).mkString.toLong

    Race(time, record)

  def part1(lines: List[String]): Long =
    val races = parse(lines)
    races.map(_.waysToBeat).product

  def part2(lines: List[String]): Long =
    val race = parseBigRace(lines)
    race.waysToBeat

  println(part1(lines))
  println(part2(lines))
