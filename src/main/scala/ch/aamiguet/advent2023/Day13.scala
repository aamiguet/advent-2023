package ch.aamiguet.advent2023

import scala.io.Source
import org.apache.commons.text.similarity.LevenshteinDistance

object Day13 extends App:

  lazy val lines = Source.fromFile("data/day13.txt").getLines().toList
  lazy val distance = LevenshteinDistance.getDefaultInstance()

  trait Reflection:
    def value: Int

  case class Horizontal(n: Int) extends Reflection:
    lazy val value = n * 100

  case class Vertical(value: Int) extends Reflection

  case class Pattern(lines: Array[String]):
    lazy val columns =
      def loop(ls: Array[String], acc: Array[String]): Array[String] =
        if ls.isEmpty then acc
        else
          val newAcc = ls.head.toCharArray.zip(acc).map((c, s) => s.appended(c))
          loop(ls.tail, newAcc)
      loop(lines, Array.fill(lines.head.size)(""))

    private def isReflection(pos: Int, ls: Array[String]): Boolean =
      val steps = math.min(ls.size - pos, pos)
      val pairs = (pos - 1 to pos - steps by -1).zip(pos until pos + steps)
      pairs.forall((i, j) => ls(i) == ls(j))

    private def isHorizontalReflection(pos: Int) = isReflection(pos, lines)

    private def isVerticalReflection(pos: Int) = isReflection(pos, columns)

    private def isCorrectedReflection(pos: Int, ls: Array[String]): Boolean =
      val steps = math.min(ls.size - pos, pos)
      val pairs = (pos - 1 to pos - steps by -1).zip(pos until pos + steps)
      pairs.map((i, j) => distance.apply(ls(i), ls(j)).toInt).sum == 1

    private def isHorizontalCorrectedReflection(pos: Int) = isCorrectedReflection(pos, lines)

    private def isVerticalCorrectedReflection(pos: Int) = isCorrectedReflection(pos, columns)

    lazy val reflection: Reflection =
      (1 until lines.size).filter(isHorizontalReflection).headOption match
        case Some(n) => Horizontal(n)
        case None =>
          val n = (1 until columns.size).filter(isVerticalReflection).head
          Vertical(n)

    lazy val correctedReflection: Reflection =
      (1 until lines.size).filter(isHorizontalCorrectedReflection).headOption match
        case Some(n) => Horizontal(n)
        case None =>
          val n = (1 until columns.size).filter(isVerticalCorrectedReflection).head
          Vertical(n)

  def summarize(rs: List[Reflection]): Int =
    rs.foldLeft(0): (acc, r) =>
      acc + r.value

  def parse(lines: List[String]): List[Pattern] =
    def loop(ls: List[String], acc: List[Pattern] = Nil): List[Pattern] =
      if ls.isEmpty then acc
      else
        ls.span(_.nonEmpty) match
          case (ps, rest) if rest.nonEmpty => loop(rest.tail, Pattern(ps.toArray) :: acc)
          case (ps, _) => Pattern(ps.toArray) :: acc
    loop(lines)

  def part1(lines: List[String]): Int =
    val patterns = parse(lines)
    val reflections = patterns
      .map: p =>
        p.reflection
    summarize(reflections)

  def part2(lines: List[String]): Int =
    val patterns = parse(lines)
    val reflections = patterns
      .map: p =>
        p.correctedReflection
    summarize(reflections)

  println(part1(lines))
  println(part2(lines))
