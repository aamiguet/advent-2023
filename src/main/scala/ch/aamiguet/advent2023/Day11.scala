package ch.aamiguet.advent2023

import scala.io.Source

object Day11 extends App:

  lazy val lines = Source.fromFile("data/day11.txt").getLines().toList

  case class Pos(x: Long, y: Long)

  def parse(lines: List[String]): List[Pos] =
    for
      (l, x) <- lines.zipWithIndex
      (c, y) <- l.zipWithIndex if c == '#'
    yield Pos(x, y)

  def expand(pos: List[Pos], ratio: Long): List[Pos] =
    val xs = pos.map(_.x).toSet
    val ys = pos.map(_.y).toSet
    val exs = (0 to xs.max.toInt).toList.filterNot(xs.contains)
    val eys = (0 to ys.max.toInt).toList.filterNot(ys.contains)
    pos.map: p =>
      val x = p.x + (exs.takeWhile(_ < p.x).size * (ratio - 1L))
      val y = p.y + (eys.takeWhile(_ < p.y).size * (ratio - 1L))
      Pos(x, y)

  def allDistances(galaxies: List[Pos], acc: List[(Pos, Pos, Long)] = Nil): List[(Pos, Pos, Long)] =
    if galaxies.isEmpty then acc
    else
      val g1 = galaxies.head
      val dist = galaxies.tail.map: g2 =>
        (g1, g2, math.abs(g1.x - g2.x) + math.abs(g1.y - g2.y))
      allDistances(galaxies.tail, acc ++ dist)

  def part(lines: List[String], expansionRatio: Long): Long =
    val galaxies = parse(lines)
    val expanded = expand(galaxies, expansionRatio)
    allDistances(expanded).map(_._3).sum

  def part1(lines: List[String]): Long =
    part(lines, 2L)

  def part2(lines: List[String]): Long =
    part(lines, 1000000L)

  println(part1(lines))
  println(part2(lines))
