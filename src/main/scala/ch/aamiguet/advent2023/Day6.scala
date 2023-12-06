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
      val r = 0L until time
      r.foldLeft(0L): (acc, t) =>
        if (time - t) * t > record then acc + 1L else acc

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
