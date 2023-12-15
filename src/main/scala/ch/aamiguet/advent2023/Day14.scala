package ch.aamiguet.advent2023

import scala.io.Source

object Day14 extends App:

  lazy val lines = Source.fromFile("data/day14.txt").getLines().toArray

  enum Ground:
    case Empty extends Ground
    case RoundRock extends Ground
    case CubeRock extends Ground
  import Ground.*

  object Ground:
    def apply(c: Char): Ground = c match
      case '.' => Empty
      case '#' => CubeRock
      case 'O' => RoundRock

  extension (g: Ground)
    def str: String = g match
      case Empty => "."
      case CubeRock => "#"
      case RoundRock => "O"

  def parse(lines: Array[String]): Array[Array[Ground]] =
    val arr = Array.fill[Array[Ground]](lines.size)(Array.fill(lines.head.size)(Empty))
    for
      i <- 0 until lines.size
      j <- 0 until lines.head.size
    yield arr(i).update(j, Ground(lines(i)(j)))
    arr

  def tiltNorth(arr: Array[Array[Ground]], free: Array[Int], currentRow: Int): Unit =
    for i <- 0 until arr(0).size
    yield arr(currentRow)(i) match
      case CubeRock => free.update(i, currentRow + 1)
      case RoundRock =>
        arr(currentRow).update(i, Empty)
        arr(free(i)).update(i, RoundRock)
        free.update(i, free(i) + 1)
      case Empty =>

  def tiltNorth(arr: Array[Array[Ground]]): Unit =
    val free = Array.fill[Int](arr(0).size)(0)
    for i <- 0 until arr.size
    yield tiltNorth(arr, free, i)

  def rotateClockwise(arr: Array[Array[Ground]]): Array[Array[Ground]] =
    val rotatedArr = Array.fill[Array[Ground]](arr(0).size)(Array.fill(arr.size)(Empty))
    for
      i <- 0 until arr.size
      j <- 0 until arr(0).size
    yield rotatedArr(j).update(arr.size - 1 - i, arr(i)(j))
    rotatedArr

  def state(arr: Array[Array[Ground]]): String =
    arr
      .map: a =>
        a.map(_.str).mkString("")
      .mkString("\n")

  def cycle(arr: Array[Array[Ground]]): Array[Array[Ground]] =
    val c = arr.map(_.map(identity))
    (0 until 4).foldLeft(c): (acc, _) =>
      tiltNorth(acc)
      rotateClockwise(acc)

  def cycle(
      arr: Array[Array[Ground]],
      n: Long,
      limit: Long,
      history: List[(String, Array[Array[Ground]])]
  ): Array[Array[Ground]] =
    val cycled = cycle(arr)
    val s = state(cycled)
    val period = history.indexWhere(_._1 == s) + 1
    if period > 0 then
      val shift = (limit - n) % period.toLong
      history.drop(period - shift.toInt - 1).head._2
    else cycle(cycled, n + 1, limit, (s, cycled) :: history)

  def load(arr: Array[Array[Ground]]): Int =
    val s = arr.size
    arr.zipWithIndex
      .map: (a, i) =>
        a.filter(_ == RoundRock).size * (s - i)
      .sum

  def part1(lines: Array[String]): Int =
    val arr = parse(lines)
    tiltNorth(arr)
    load(arr)

  def part2(lines: Array[String]): Int =
    val arr = parse(lines)
    val finalState = cycle(arr, 1L, 1000000000L, Nil)
    load(finalState)

  println(part1(lines))
  println(part2(lines))
