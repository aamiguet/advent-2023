package ch.aamiguet.advent2023

import scala.io.Source
import scala.collection.immutable.NumericRange.Exclusive

object Day5 extends App:

  lazy val lines = Source.fromFile("data/day5.txt").getLines().toList

  val pattern = "\\d+".r

  case class Rule(
      destinationStart: Long,
      sourceStart: Long,
      range: Long
  ):
    lazy val shift = destinationStart - sourceStart

    def destination(source: Long): Option[Long] =
      val delta = source - sourceStart
      if delta >= 0 && delta < range then Some(destinationStart + delta)
      else None

    private def nextExclusiveRanges(
        er: Exclusive[Long]
    ): (List[Exclusive[Long]], List[Exclusive[Long]]) =
      val candStart = math.max(sourceStart, er.start)
      val candEnd = math.min(sourceStart + range, er.end)
      if candEnd > candStart then
        val modified = candStart + shift until candEnd + shift
        val unchangedLeft =
          if candStart > er.start then Some(er.start until candStart)
          else None
        val unchangedRight =
          if candEnd < er.end then Some(candEnd until er.end)
          else None
        (List(unchangedLeft, unchangedRight).flatten, List(modified))
      else (List(er), Nil)

    def nextExclusiveRanges(
        ers: List[Exclusive[Long]]
    ): (List[Exclusive[Long]], List[Exclusive[Long]]) =
      ers
        .map(nextExclusiveRanges)
        .foldLeft((List.empty[Exclusive[Long]], List.empty[Exclusive[Long]])): (acc, e) =>
          (acc._1 ++ e._1, acc._2 ++ e._2)

  object Rule:
    def apply(s: String): Rule =
      val alls = pattern.findAllIn(s).map(_.toLong).toArray
      Rule(
        alls(0),
        alls(1),
        alls(2)
      )

  case class Ruleset(
      name: String,
      rules: List[Rule]
  ):
    def destination(source: Long): Long =
      rules.map(_.destination(source)).flatten.headOption.getOrElse(source)

    def nextExclusiveRanges(ers: List[Exclusive[Long]]): List[Exclusive[Long]] =
      val (unchanged, modified) = rules.foldLeft(ers, List.empty[Exclusive[Long]]): (acc, rule) =>
        val (u, m) = rule.nextExclusiveRanges(acc._1)
        (u, acc._2 ++ m)
      unchanged ++ modified

  case class Almanac(
      seeds: List[Long],
      ers: List[Exclusive[Long]],
      rulesets: List[Ruleset]
  ):
    lazy val locations: List[Long] =
      seeds.map: s =>
        rulesets.foldLeft(s)((acc, r) => r.destination(acc))

    lazy val locationRanges: List[Exclusive[Long]] =
      rulesets.foldLeft(ers)((acc, r) => r.nextExclusiveRanges(acc))

  object Almanac:
    def apply(lines: List[String]): Almanac =
      def loop(lines: List[String], acc: List[Ruleset] = Nil): List[Ruleset] =
        if lines.isEmpty then acc.reverse
        else
          val (m, rest) = lines.tail.span(_.nonEmpty)
          val rs =
            Ruleset(
              m.head,
              m.tail.map(Rule(_))
            )
          loop(rest, rs :: acc)

      val seeds = pattern.findAllIn(lines.head).map(_.toLong).toList
      val ranges: List[Exclusive[Long]] =
        seeds.grouped(2).toList.map(p => p.head until p.head + p.last)
      Almanac(
        seeds,
        ranges,
        loop(lines.tail)
      )

  def part1(lines: List[String]): Long =
    val almanac = Almanac(lines)
    almanac.locations.min

  def part2(lines: List[String]): Long =
    val almanac = Almanac(lines)
    almanac.locationRanges.map(_.start).min

  println(part1(lines))
  println(part2(lines))
