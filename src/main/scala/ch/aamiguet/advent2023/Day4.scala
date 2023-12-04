package ch.aamiguet.advent2023

import scala.io.Source

object Day4 extends App:

  lazy val lines = Source.fromFile("data/day4.txt").getLines().toList.filter(_.nonEmpty)

  case class Card(
    id: Int,
    winningNumbers: Set[Int],
    numbers: Set[Int],
  ):
    lazy val matches: Int =
      (winningNumbers intersect numbers).size

    lazy val score: Int =
      if matches > 0 then math.pow(2, matches - 1).toInt
      else 0

  object Card:
    val pattern = "\\d+".r

    private def allNumbers(s: String): Set[Int] =
      pattern.findAllIn(s).map(_.toInt).toSet

    def apply(s: String): Card =
      val s1 = s.split(":")
      val ns = s1(1).split("\\|")
      Card(
        allNumbers(s1(0)).head,
        allNumbers(ns(0)),
        allNumbers(ns(1)),
      )

  def part1(lines: List[String]): Int =
    lines.map(Card(_)).map(_.score).sum

  def part2(lines: List[String]): Int =
    val startingStack = lines.map(Card(_))
    val ms = startingStack.map(_.matches).toArray
    val r = 0 until startingStack.size
    val f = r.foldLeft((0, Array.fill(startingStack.size)(0))): (acc, i) =>
      val m = ms(i)
      val count = 1 + acc._2(i)
      val newCopies = (i + 1 to i + m).foldLeft(acc._2): (acc, i) =>
        acc.updated(i, acc(i) + count)
      (acc._1 + count, newCopies)
    f._1

  println(part1(lines))
  println(part2(lines))
