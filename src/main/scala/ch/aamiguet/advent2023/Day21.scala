package ch.aamiguet.advent2023

import scala.io.Source

class Day21(lines: Array[String]):

  enum Tile:
    case Rock, Garden, StartingGarden
  import Tile.*

  case class Pos(x: Int, y: Int)

  lazy val xSize = lines.size
  lazy val ySize = lines.head.size

  lazy val grid =
    val g = for
      i <- 0 until xSize
      j <- 0 until ySize
      t = lines(i)(j) match
        case '#' => Rock
        case 'S' => StartingGarden
        case _ => Garden
    yield Pos(i, j) -> t
    g.toMap

  lazy val startPos =
    grid.find(_._2 == StartingGarden).get._1

  lazy val nextStepMap = scala.collection.mutable.Map.empty[Pos, Set[Pos]]

  private def nextStep(pos: Pos): Set[Pos] =
    nextStepMap.getOrElseUpdate(
      pos, {
        val cands = Set(
          pos.copy(x = pos.x - 1),
          pos.copy(x = pos.x + 1),
          pos.copy(y = pos.y - 1),
          pos.copy(y = pos.y + 1)
        )
        cands.filter(p => grid.isDefinedAt(p) && grid(p) != Rock)
      }
    )

  def finalPos(steps: Int, acc: Set[Pos] = Set(startPos)): Set[Pos] =
    if steps == 0 then acc
    else finalPos(steps - 1, acc.flatMap(nextStep))

  def plotCount(steps: Int) = finalPos(steps).size

  lazy val part1: Int = plotCount(64)
  lazy val part2: Int = ???

object Day21 extends App:

  lazy val lines = Source.fromFile("data/day21.txt").getLines().toArray

  val d = Day21(lines)
  println(d.part1)
