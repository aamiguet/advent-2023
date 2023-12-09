package ch.aamiguet.advent2023

import scala.io.Source

object Day9 extends App:

  lazy val lines = Source.fromFile("data/day9.txt").getLines().toList

  val pattern = "-?\\d+".r

  private def lastValues(increments: List[Int], acc: List[Int] = Nil): List[Int] =
    if increments.forall(_ == 0) then 0 :: acc
    else
      val diffs = increments.zip(increments.tail).map((a, b) => b - a)
      lastValues(diffs, increments.last :: acc)

  private def extrapolatedValue(line: String): Int =
    val readings = pattern.findAllIn(line).map(_.toInt).toList
    val lvs = lastValues(readings)
    val evs = lvs.foldLeft(List.empty[Int]): (acc, lv) =>
      acc.headOption.getOrElse(0) + lv :: acc
    evs.head

  private def firstValues(increments: List[Int], acc: List[Int] = Nil): List[Int] =
    if increments.forall(_ == 0) then 0 :: acc
    else
      val diffs = increments.zip(increments.tail).map((a, b) => b - a)
      firstValues(diffs, increments.head :: acc)

  private def extrapolatedFirstValue(line: String): Int =
    val readings = pattern.findAllIn(line).map(_.toInt).toList
    val fvs = firstValues(readings)
    val evs = fvs.foldLeft(List.empty[Int]): (acc, fv) =>
      fv - acc.headOption.getOrElse(0) :: acc
    evs.head

  def part1(lines: List[String]): Int =
    lines.map(extrapolatedValue(_)).sum

  def part2(lines: List[String]): Int =
    lines.map(extrapolatedFirstValue(_)).sum

  println(part1(lines))
  println(part2(lines))
