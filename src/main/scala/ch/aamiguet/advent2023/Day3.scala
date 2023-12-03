package ch.aamiguet.advent2023

import scala.io.Source
import scala.util.matching.Regex.Match

object Day3 extends App:

  lazy val lines = Source.fromFile("data/day3.txt").getLines().toList

  lazy val pattern = "\\d+".r
  lazy val gearPattern = "\\*".r

  private def isSymbol(c: Char): Boolean =
    !c.isDigit && c != '.'

  private def isPartNumber(line: Int, m: Match, schematic: Array[String]): Boolean =
    val adjacents =
      List((line, m.start - 1), (line, m.end)) ++
        (for
          l <- List(line - 1, line + 1)
          c <- m.start - 1 to m.end
        yield (l, c))
    adjacents.exists: a =>
      schematic.lift(a._1).flatMap(_.lift(a._2)).map(isSymbol).getOrElse(false)

  private def partNumbers(line: Int, schematic: Array[String]): List[Int] =
    val candidates = pattern.findAllMatchIn(schematic(line)).toList
    candidates.filter(isPartNumber(line, _, schematic)).map(_.matched.toInt)

  def part1(lines: List[String]): Int =
    val schematic = lines.toArray
    val pns =
      for line <- (0 until schematic.size).toList
      yield partNumbers(line, schematic)
    pns.flatten.sum

  private def gearScore(index: Int, gearMatch: Match, numberMatches: Array[List[Match]]): Int =
    val pos = gearMatch.start - 1 to gearMatch.end
    val over = numberMatches
      .lift(index - 1)
      .getOrElse(Nil)
      .filter: m =>
        ((m.start until m.end) intersect pos).nonEmpty
    val under = numberMatches
      .lift(index + 1)
      .getOrElse(Nil)
      .filter: m =>
        ((m.start until m.end) intersect pos).nonEmpty
    val same = numberMatches(index)
      .filter: m =>
        m.end == gearMatch.start || m.start == gearMatch.end
    val all = over ++ under ++ same
    if all.size == 2 then all.head.matched.toInt * all.last.matched.toInt
    else 0

  private def gearScore(index: Int, line: String, numberMatches: Array[List[Match]]): Int =
    val candidates = gearPattern.findAllMatchIn(line).toList
    candidates.map(gearScore(index, _, numberMatches)).sum

  def part2(lines: List[String]): Int =
    val numberMatches = lines.toArray.map(pattern.findAllMatchIn(_).toList)
    lines.zipWithIndex.map(li => gearScore(li._2, li._1, numberMatches)).sum

  println(part1(lines))
  println(part2(lines))
