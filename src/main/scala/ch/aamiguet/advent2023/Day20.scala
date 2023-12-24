package ch.aamiguet.advent2023

import scala.io.Source

class Day20(lines: List[String]):

  enum Pulse:
    case High extends Pulse
    case Low extends Pulse
  import Pulse.*

  case class Message(destination: String, source: String, pulse: Pulse)

  trait Module:
    def name: String
    def destinations: List[String]
    protected def send(pulse: Pulse): List[Message] =
      destinations.map(Message(_, name, pulse))
    def receive(message: Message): List[Message]

  case class Broadcaster(destinations: List[String]) extends Module:
    lazy val name = "broadcaster"

    def receive(message: Message): List[Message] =
      send(message.pulse)

  case class FlipFlop(name: String, destinations: List[String]) extends Module:
    private var on = false

    def receive(message: Message): List[Message] = message.pulse match
      case High => Nil
      case Low =>
        val p =
          if on then
            on = false
            Low
          else
            on = true
            High
        send(p)

  case class Conjunction(name: String, destinations: List[String]) extends Module:
    private val received = scala.collection.mutable.Map.empty[String, Pulse]

    def receive(message: Message): List[Message] =
      received.update(message.source, message.pulse)
      if received.forall(_._2 == High) then send(Low)
      else send(High)

    def init(source: String): Unit =
      received.update(source, Low)

  def parse(lines: List[String]): Map[String, Module] =
    def parse(line: String): (String, Module) =
      val s = line.split(" -> ")
      val dests = s(1).split(", ").toList
      s(0) match
        case "broadcaster" => s(0) -> Broadcaster(dests)
        case s"%$name" => name -> FlipFlop(name, dests)
        case s"&$name" => name -> Conjunction(name, dests)
    lines.map(parse).toMap

  def initModuleMap(lines: List[String]): Map[String, Module] =
    val moduleMap = parse(lines)
    for
      sm <- moduleMap.values
      dest <- sm.destinations
      dm <- moduleMap.get(dest)
    yield if dm.isInstanceOf[Conjunction] then dm.asInstanceOf[Conjunction].init(sm.name)
    moduleMap

  lazy val part1: Long =
    val moduleMap = initModuleMap(lines)

    def process(messages: List[Message], lowCount: Long = 0L, highCount: Long = 0L): (Long, Long) =
      if messages.isEmpty then (lowCount, highCount)
      else
        val m = messages.head
        val (l, h) = if m.pulse == Low then (lowCount + 1, highCount) else (lowCount, highCount + 1)
        val next = moduleMap.get(m.destination) match
          case Some(module) => module.receive(m)
          case None => Nil
        process(messages.tail ++ next, l, h)

    val buttonPush = (0 until 1000).toList
    val finalCount = buttonPush.foldLeft((0L, 0L)): (acc, _) =>
      val (l, h) = process(List(Message("broadcaster", "button", Low)))
      (acc._1 + l, acc._2 + h)

    finalCount._1 * finalCount._2

  lazy val part2: Long =
    val moduleMap = initModuleMap(lines)

    def process(messages: List[Message] = Nil, buttonPush: Long = 0L): Long =
      if messages.isEmpty then process(List(Message("broadcaster", "button", Low)), buttonPush + 1)
      else
        val m = messages.head
        if m.destination == "rx" && m.pulse == Low then buttonPush
        else
          val next = moduleMap.get(m.destination) match
            case Some(module) => module.receive(m)
            case None => Nil
          process(messages.tail ++ next, buttonPush)
    process()

object Day20 extends App:

  lazy val lines = Source.fromFile("data/day20.txt").getLines().toList

  val d = Day20(lines)

  println(d.part1)
  println(d.part2)
