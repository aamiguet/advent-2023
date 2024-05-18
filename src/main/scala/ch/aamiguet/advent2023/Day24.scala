package ch.aamiguet.advent2023

import scala.io.Source

class Day24(lines: List[String]):

  case class Position(x: Double, y: Double, z: Double)
  case class Velocity(x: Double, y: Double, z: Double)

  case class Hailstone(
      pos: Position,
      velocity: Velocity
  ):
    lazy val a = -velocity.y
    lazy val b = velocity.x
    lazy val c = a * pos.x + b * pos.y

    def isParallel(other: Hailstone): Boolean =
      velocity.y * other.velocity.z - velocity.z * other.velocity.y == 0 &&
        velocity.z * other.velocity.x - velocity.x * other.velocity.z == 0 &&
        velocity.x * other.velocity.y - velocity.y * other.velocity.x == 0

    def isFuture(pos: Position): Boolean =
      (pos.x - this.pos.x) / velocity.x > 0d

    def crossingPosition(other: Hailstone): Option[Position] =
      val det = a * other.b - other.a * b
      if det == 0 then None
      else
        val x = (c * other.b - other.c * b) / det
        val y = (a * other.c - other.a * c) / det
        Some(Position(x, y, 0d))

    def futureCrossingPosition(other: Hailstone): Option[Position] =
      crossingPosition(other).filter(p => isFuture(p) && other.isFuture(p))

  object Hailstone:
    def apply(s: String): Hailstone =
      val s1 = s.split(" @ ")
      val p =
        s1(0).split(", ").map(_.toDouble) match
          case Array(x, y, z) => Position(x, y, z)
      val v =
        s1(1).split(", ").map(_.toDouble) match
          case Array(x, y, z) => Velocity(x, y, z)
      Hailstone(p, v)

  def crossingFuturePathCount(min: Double, max: Double): Int =
    def loop(hs: List[Hailstone], acc: Int = 0): Int =
      if hs.isEmpty then acc
      else
        val s = hs.tail
          .flatMap(hs.head.futureCrossingPosition(_))
          .filter(p => p.x >= min && p.x <= max && p.y >= min && p.y <= max)
          .size
        loop(hs.tail, acc + s)
    loop(hailstones)

  lazy val hailstones = lines.map(Hailstone(_))

  lazy val parallelHailstones: List[(Hailstone, Hailstone)] =
    def loop(
        hs: List[Hailstone],
        acc: List[(Hailstone, Hailstone)] = List.empty
    ): List[(Hailstone, Hailstone)] =
      if hs.isEmpty then acc
      else
        val pairs = hs.tail
          .filter(hs.head.isParallel(_))
          .map((hs.head, _))
        loop(hs.tail, acc ++ pairs)
    loop(hailstones)

  def areCoplanar(h1: Hailstone, h2: Hailstone, h3: Hailstone): Boolean =
    val det = h1.velocity.x * h2.velocity.y * h3.velocity.z +
      h2.velocity.x * h3.velocity.y * h1.velocity.z +
      h3.velocity.x * h1.velocity.y * h2.velocity.z -
      h1.velocity.z * h2.velocity.y * h3.velocity.x -
      h2.velocity.z * h3.velocity.y * h1.velocity.x -
      h3.velocity.z * h1.velocity.y * h2.velocity.x
    det == 0

  lazy val coplanarHailstones: List[(Hailstone, Hailstone, Hailstone)] =
    def loop(
        hs: List[Hailstone],
        acc: List[(Hailstone, Hailstone, Hailstone)] = List.empty
    ): List[(Hailstone, Hailstone, Hailstone)] =
      if hs.tail.isEmpty then acc
      else
        val triplets = hs.tail.tail
          .filter(areCoplanar(hs.head, hs.tail.head, _))
          .map((hs.head, hs.tail.head, _))
        loop(hs.tail, acc ++ triplets)
    loop(hailstones)

  lazy val part1: Int = crossingFuturePathCount(200000000000000d, 400000000000000d)
  lazy val part2: Int =
    println(coplanarHailstones mkString "\n")
    ???

object Day24 extends App:

  lazy val lines = Source.fromFile("data/day24.txt").getLines().toList

  val d = Day24(lines)
  println(d.part1)
  println(d.part2)
