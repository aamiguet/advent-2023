package ch.aamiguet.advent2023

import scala.io.Source

object Day2 extends App:

  lazy val lines = Source.fromFile("data/day2.txt").getLines().toList

  case class CubeSet(
    red: Int,
    green: Int,
    blue: Int,
  ):
    def isPossible(cubeSet: CubeSet): Boolean =
      cubeSet.red >= red && cubeSet.green >= green && cubeSet.blue >= blue

  object CubeSet:
    def apply(cubeSet: String): CubeSet =
      val m = cubeSet
        .split(", ")
        .map:
          s => val vk = s.split(" ")
          vk(1) -> vk(0).toInt
        .toMap
        .withDefaultValue(0)

      CubeSet(
        m("red"),
        m("green"),
        m("blue"),
      )

  case class Game(
    id: Int,
    cubes: List[CubeSet],
  ):
    def isPossible(cubeSet: CubeSet): Boolean =
      cubes.forall(_.isPossible(cubeSet))

    def power: Int =
      val minCubeSet = cubes.foldLeft(CubeSet(0, 0, 0)): (acc, c) =>
        CubeSet(math.max(acc.red, c.red), math.max(acc.green, c.green), math.max(acc.blue, c.blue))
      minCubeSet.red * minCubeSet.green * minCubeSet.blue

  object Game:
    def apply(game: String): Game =
      val s = game.split(": ")
      Game(
        s(0).split(" ")(1).toInt,
        s(1).split("; ").toList.map(CubeSet(_)),
      )

  def part1(lines: List[String]): Int =
    val maxCubeSet = CubeSet(12, 13, 14)
    val games = lines.map(Game(_))
    games.filter(_.isPossible(maxCubeSet)).map(_.id).sum

  def part2(lines: List[String]): Int =
    val games = lines.map(Game(_))
    games.map(_.power).sum

  println(part1(lines))
  println(part2(lines))
