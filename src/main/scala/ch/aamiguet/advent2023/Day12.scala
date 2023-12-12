package ch.aamiguet.advent2023

import scala.io.Source

object Day12 extends App:

  lazy val lines = Source.fromFile("data/day12.txt").getLines().toList

  lazy val arrMap =
    scala.collection.mutable.Map.empty[Record, Long]

  case class Record(
      springs: String,
      groups: List[Int],
      currentDamagedCount: Option[Int] = None
  ):
    lazy val isGroupComplete = currentDamagedCount match
      case None => groups.isEmpty
      case Some(c) => groups.size == 1 && c == groups.head

    lazy val isArrangement: Boolean =
      springs.isEmpty && isGroupComplete

    def nextRecords: List[Record] =
      if springs.isEmpty then Nil
      else if springs.head == '?' then
        List(this.copy(springs = "." + springs.tail), this.copy(springs = "#" + springs.tail))
      else if springs.head == '.' then
        currentDamagedCount match
          case Some(c) if groups.isEmpty => Nil
          case Some(c) if groups.head != c => Nil
          case Some(c) => List(Record(springs.tail, groups.tail, None))
          case None => List(Record(springs.tail, groups, None))
      else
        currentDamagedCount match
          case Some(c) if groups.head == c => Nil
          case Some(c) => List(Record(springs.tail, groups, Some(c + 1)))
          case None if groups.isEmpty => Nil
          case None => List(Record(springs.tail, groups, Some(1)))

    def unfold: Record =
      val r = (0 until 5).toList
      this.copy(
        springs = r.map(_ => springs).mkString("?"),
        groups = r.flatMap(_ => groups)
      )

  object Record:
    def apply(s: String): Record =
      val s1 = s.split(" ")
      Record(
        s1(0),
        s1(1).split(",").map(_.toInt).toList
      )

  private def arrangements0(record: Record): Long =
    if record.isArrangement then 1L
    else record.nextRecords.map(arrangements).sum

  private def arrangements(record: Record): Long =
    arrMap.getOrElseUpdate(record, arrangements0(record))

  def part1(lines: List[String]): Long =
    val records = lines.map(Record(_))
    records.map(arrangements(_)).sum

  def part2(lines: List[String]): Long =
    val records = lines.map(Record(_).unfold)
    records.map(arrangements0(_)).sum

  println(part1(lines))
  println(part2(lines))
