package ch.aamiguet.advent2023

import scala.io.Source

object Day15 extends App:

  lazy val lines = Source.fromFile("data/day15.txt").getLines().toList

  case class Lens(
      label: String,
      focus: Int
  )

  trait Operation
  case class Remove(label: String) extends Operation
  case class Add(lens: Lens) extends Operation

  def hash(s: String): Int =
    s.foldLeft(0): (acc, c) =>
      ((acc + c) * 17) % 256

  private def remove(boxes: Map[Int, List[Lens]], label: String): Map[Int, List[Lens]] =
    val box = hash(label)
    boxes.updatedWith(box):
      case Some(ls) => Some(ls.filter(_.label != label))
      case None => None

  private def add(boxes: Map[Int, List[Lens]], lens: Lens): Map[Int, List[Lens]] =
    val box = hash(lens.label)
    boxes.updatedWith(box):
      case Some(ls) if ls.exists(l => l.label == lens.label) =>
        Some(ls.map(l => if l.label == lens.label then lens else l))
      case Some(ls) => Some(lens :: ls)
      case None => Some(List(lens))

  private def initialize(
      ops: List[Operation],
      boxes: Map[Int, List[Lens]] = Map.empty[Int, List[Lens]]
  ): Map[Int, List[Lens]] =
    if ops.isEmpty then boxes
    else
      val updatedBoxes = ops.head match
        case Remove(label) => remove(boxes, label)
        case Add(lens) => add(boxes, lens)
      initialize(ops.tail, updatedBoxes)

  private def parse(line: String): List[Operation] =
    line
      .split(",")
      .toList
      .map: s =>
        if s.last == '-' then Remove(s.dropRight(1))
        else
          s.split("=").toList match
            case label :: focus :: Nil => Add(Lens(label, focus.toInt))
            case _ => throw Error("Not an operation")

  def part1(line: String): Int =
    line.split(",").map(hash).sum

  def part2(line: String): Int =
    val ops = parse(line)
    val boxes = initialize(ops)
    val power = boxes.map: (box, lenses) =>
      if lenses.isEmpty then 0
      else
        lenses.reverse.zipWithIndex
          .map: (lens, slot) =>
            (box + 1) * (slot + 1) * lens.focus
          .sum
    power.sum

  println(part1(lines.head))
  println(part2(lines.head))
