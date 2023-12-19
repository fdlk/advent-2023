import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day19.txt"))

type WorkflowName = String
type XMAS = Char

case class Part(x: Int, m: Int, a: Int, s: Int)

case class Rule(property: XMAS, comparison: Char, value: Int, routing: String) {
  def matches(part: Part): Boolean = {
    val partValue = property match {
      case 'x' => part.x
      case 'm' => part.m
      case 'a' => part.a
      case 's' => part.s
    }
    comparison match {
      case '>' => partValue > value
      case '<' => partValue < value
    }
  }

  def edge: Int = comparison match {
    case '>' => value + 1
    case '<' => value
  }
}

case class Workflow(name: WorkflowName, rules: List[Rule], default: WorkflowName) {
  def route(part: Part): String = rules.find(_.matches(part)).map(_.routing).getOrElse(default)
}

val regex = """([xmas])([<>])(.+):(.+)""".r
def parseRule(rule: String): Rule = rule match {
  case regex(property, comparison, value, routing) =>
    Rule(property.charAt(0), comparison.charAt(0), value.toInt, routing)
}

val workflowRegex = """(.+)\{(.*),([^,]+)}""".r
def parseWorkflow(line: String): (String, Workflow) = line match {
  case workflowRegex(name, rules, default) => name -> Workflow(name, rules.split(",").map(parseRule).toList, default)
}
val workflows = input.takeWhile(line => !line.isBlank).map(parseWorkflow).toMap

def parsePart(line: String): Part = line match {
  case s"{x=${x},m=${m},a=${a},s=${s}}" => Part(x.toInt, m.toInt, a.toInt, s.toInt)
}
val parts = input.dropWhile(line => !line.isBlank).tail.map(parsePart)

@tailrec
def accept(part: Part, workflow: String): Boolean = {
  workflows(workflow).route(part) match {
    case "A" => true
    case "R" => false
    case other => accept(part, other)
  }
}

val part1 = parts.filter(accept(_, "in")).map(part => part.x + part.m + part.a + part.s).sum

def splitRange(range: Range, edge: Int): List[Range] =
  if (!range.contains(edge)) List(range)
  else List(range.start until edge, edge to range.last)

type State = Map[XMAS, Range]
def numParts(ranges: State): Long = ranges.values.map(_.size.toLong).product

def part(ranges: State): Part = Part(ranges('x').start, ranges('m').start, ranges('a').start, ranges('s').start)

def split(ranges: State, rule: Rule): (State, State) = {
  val (yes, no) = splitRange(ranges(rule.property), rule.edge)
    .map(range => ranges.updated(rule.property, range))
    .partition(ranges => rule.matches(part(ranges)))
  (yes.head, no.head)
}

@tailrec
def numAcceptable(options: List[(State, WorkflowName)], soFar: Long): Long = options match {
  case Nil => soFar
  case (ranges, "A") :: rest => numAcceptable(rest, soFar + numParts(ranges))
  case (_, "R") :: rest => numAcceptable(rest, soFar)
  case (ranges, workflowName) :: rest =>
    val Workflow(_, rules, default) = workflows(workflowName)
    val (unroutedState, routings) = rules.foldLeft[(State, List[(State, WorkflowName)])]((ranges, List()))({
      case ((ranges, routed), rule) =>
        val (yes, no) = split(ranges, rule)
        (no, (yes, rule.routing) :: routed)
    })
    numAcceptable((unroutedState, default) :: routings ::: rest, soFar)
}

numAcceptable(List(("xmas".map(_ -> (1 to 4000)).toMap, "in")), 0)