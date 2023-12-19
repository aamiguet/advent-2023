package ch.aamiguet.advent2023

import scala.io.Source

class Day19(lines: List[String]):

  lazy val (workflows: Map[String, Workflow], parts: List[Part]) =
    val (wlines, plines) = lines.span(_.nonEmpty)
    val ws = wlines.map: l =>
      val s = l.dropRight(1).split("[{]")
      (s(0) -> Workflow(s(1)))
    val ps = plines.tail.map(Part(_))
    (ws.toMap, ps)

  enum Result:
    case ApplyRule(name: String) extends Result
    case Accepted extends Result
    case Rejected extends Result
  import Result.*

  case class Condition(
      rating: Char,
      operator: Char,
      value: Int
  )

  object Condition:
    def apply(s: String): Condition =
      Condition(s.head, s.tail.head, s.drop(2).toInt)

  case class Rule(
      condition: Condition,
      result: Result
  )

  case class Workflow(
      rules: List[Rule],
      default: Result
  )

  object Workflow:
    def apply(s: String): Workflow =
      def result(r: String): Result = r match
        case "R" => Rejected
        case "A" => Accepted
        case name => ApplyRule(name)

      def rule(r: String): Rule =
        val s1 = r.split(":")
        Rule(Condition(s1(0)), result(s1(1)))

      val s1 = s.split(",").toList
      val rules = s1
        .dropRight(1)
        .foldRight(List.empty[Rule]): (s, acc) =>
          rule(s) :: acc
      Workflow(rules, result(s1.last))

  case class Part(
      ratings: Map[Char, Int]
  ):
    lazy val value = ratings.values.sum

    def applyRule(rule: Rule): Option[Result] =
      rule.condition.operator match
        case '<' if ratings(rule.condition.rating) < rule.condition.value => Some(rule.result)
        case '>' if ratings(rule.condition.rating) > rule.condition.value => Some(rule.result)
        case _ => None

    def applyWorkflow(workflow: Workflow): Result =
      def loop(rules: List[Rule]): Result =
        if rules.isEmpty then
          workflow.default match
            case ApplyRule(name) => applyWorkflow(workflows(name))
            case _ => workflow.default
        else
          applyRule(rules.head) match
            case Some(result) =>
              result match
                case ApplyRule(name) => applyWorkflow(workflows(name))
                case _ => result
            case None => loop(rules.tail)
      loop(workflow.rules)

  object Part:
    def apply(s: String): Part =
      val ratings = s
        .drop(1)
        .dropRight(1)
        .split(",")
        .map: p =>
          val p1 = p.split("=")
          p1(0).head -> p1(1).toInt
      Part(ratings.toMap)

  lazy val part1: Int =
    val in = workflows("in")
    parts.filter(_.applyWorkflow(in) == Accepted).map(_.value).sum

object Day19 extends App:

  lazy val lines = Source.fromFile("data/day19.txt").getLines().toList

  val d = Day19(lines)

  println(d.part1)
