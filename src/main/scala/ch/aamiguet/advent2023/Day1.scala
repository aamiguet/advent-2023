package ch.aamiguet.advent2023

import scala.io.Source
import scala.util.matching.Regex

object Day1 extends App:

  lazy val lines = Source.fromFile("data/day1.txt").getLines().toList

  lazy val pattern1 = "([0-9])".r
  lazy val pattern2 = "one|two|three|four|five|six|seven|eight|nine|[0-9]".r

  def mapping2(s: String) = s match
    case "one" => "1"
    case "two" => "2"
    case "three" => "3"
    case "four" => "4"
    case "five" => "5"
    case "six" => "6"
    case "seven" => "7"
    case "eight" => "8"
    case "nine" => "9"
    case d => d

  extension (r: Regex)
    def findAllWithOverlap(s: String): List[String] =
      def loop(s: String, acc: List[String] = Nil): List[String] =
        r.findFirstMatchIn(s) match
          case None => acc.reverse
          case Some(m) => loop(s.drop(m.start + 1), m.matched :: acc)
      loop(s)

  def calibrationValue(pattern: Regex, mapping: String => String)(line: String): Option[Int] =
    val m = pattern.findAllWithOverlap(line)
    for
      f <- m.headOption.map(mapping)
      l <- m.lastOption.map(mapping)
    yield (f + l).toInt

  def sum(ls: List[Option[Int]]): Int =
    ls.flatten.sum

  def part1(lines: List[String]): Int =
    val cvMap = calibrationValue(pattern1, identity[String])
    sum(lines.map(cvMap))

  def part2(lines: List[String]): Int =
    val cvMap = calibrationValue(pattern2, mapping2)
    sum(lines.map(cvMap))

  println(part1(lines))
  println(part2(lines))
