package ch.aamiguet.advent2023

import scala.io.Source

object Day7 extends App:

  lazy val lines = Source.fromFile("data/day7.txt").getLines().toList

  enum Rank(val order: Int):
    case FiveOfAKind extends Rank(7)
    case FourOfAKind extends Rank(6)
    case FullHouse extends Rank(5)
    case ThreeOfAKind extends Rank(4)
    case TwoPair extends Rank(3)
    case OnePair extends Rank(2)
    case HighCard extends Rank(1)

  case class Hand(
      cards: String,
      bid: Int
  ):
    import Rank.*

    private def rank0(distinctCards: Int, maxRepetition: Int): Rank = distinctCards match
      case 1 => FiveOfAKind
      case 2 =>
        maxRepetition match
          case 4 => FourOfAKind
          case 3 => FullHouse
      case 3 =>
        maxRepetition match
          case 3 => ThreeOfAKind
          case 2 => TwoPair
      case 4 => OnePair
      case 5 => HighCard

    lazy val rank: Rank =
      val m = cards.groupBy(identity).view.mapValues(_.size)
      rank0(m.size, m.values.max)

    lazy val rankWithJoker: Rank =
      val m = cards.groupBy(identity).view.mapValues(_.size)
      val replaced =
        if m.contains('J') then
          val c = m('J')
          val removed = m.filter(_._1 != 'J')
          if removed.isEmpty then Map('A' -> 5)
          else
            val max = removed.values.max
            val cand = removed.filter(_._2 == max).keys.toList
            val highestCard = cand.sorted(using Hand.cardOrdering).head
            removed.map((k, v) => if k == highestCard then k -> (v + c) else k -> v)
        else m
      rank0(replaced.size, replaced.toMap.values.max)

  object Hand:
    def apply(s: String): Hand =
      val split = s.split(" ")
      Hand(
        split(0),
        split(1).toInt
      )

    private def compareFaceCard(x: Char, y: Char): Int = (x, y) match
      case ('A', _) => 1
      case (_, 'A') => -1
      case ('K', _) => 1
      case (_, 'K') => -1
      case ('Q', _) => 1
      case (_, 'Q') => -1
      case ('J', _) => 1
      case (_, 'J') => -1

    private def compareCard(x: Char, y: Char): Int =
      if x.isDigit || y.isDigit then x - y
      else compareFaceCard(x, y)

    private def compareCards(x: String, y: String): Int =
      if x.isEmpty then 0
      else if x.head == y.head then compareCards(x.tail, y.tail)
      else compareCard(x.head, y.head)

    val cardOrdering = new Ordering[Char] {
      override def compare(x: Char, y: Char): Int =
        compareCard(x, y)
    }

    private def compareCardsWithJoker(x: String, y: String): Int =
      if x.isEmpty then 0
      else if x.head == y.head then compareCardsWithJoker(x.tail, y.tail)
      else if x.head == 'J' then -1
      else if y.head == 'J' then 1
      else compareCard(x.head, y.head)

    given Ordering[Hand] = new Ordering[Hand]:
      override def compare(x: Hand, y: Hand): Int =
        if x.rank == y.rank then compareCards(x.cards, y.cards)
        else x.rank.order - y.rank.order

    val orderingWithJoker = new Ordering[Hand]:
      override def compare(x: Hand, y: Hand): Int =
        if x.rankWithJoker == y.rankWithJoker then compareCardsWithJoker(x.cards, y.cards)
        else x.rankWithJoker.order - y.rankWithJoker.order

  def part1(lines: List[String]): Int =
    val hands = lines.map(Hand(_))
    hands.sorted.zipWithIndex
      .map: (h, i) =>
        (i + 1) * h.bid
      .sum

  def part2(lines: List[String]): Int =
    val hands = lines.map(Hand(_))
    hands
      .sorted(using Hand.orderingWithJoker)
      .zipWithIndex
      .map: (h, i) =>
        (i + 1) * h.bid
      .sum

  println(part1(lines))
  println(part2(lines))
