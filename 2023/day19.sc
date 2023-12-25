import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day19.txt"))

type WorkflowName = String
type XMAS = Char

case class Part(x: Int, m: Int, a: Int, s: Int)

case class Rule(property: XMAS, comparison: Char, value: Int, destination: String) {
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
  def route(part: Part): String = rules.find(_.matches(part)).map(_.destination).getOrElse(default)
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

type Orthope = Map[XMAS, Range]
def numParts(orthope: Orthope): Long = orthope.values.map(_.size.toLong).product

def part(orthope: Orthope): Part = Part(orthope('x').start, orthope('m').start, orthope('a').start, orthope('s').start)

def split(orthope: Orthope, rule: Rule): (Orthope, Orthope) = {
  val (yes, no) = splitRange(orthope(rule.property), rule.edge)
    .map(range => orthope.updated(rule.property, range))
    .partition(ranges => rule.matches(part(ranges)))
  (yes.head, no.head)
}

@tailrec
def numAcceptable(orthopes: List[(Orthope, WorkflowName)], soFar: Long = 0): Long = orthopes match {
  case Nil => soFar
  case (orthope, "A") :: rest => numAcceptable(rest, soFar + numParts(orthope))
  case (_, "R") :: rest => numAcceptable(rest, soFar)
  case (orthope, workflowName) :: rest =>
    val Workflow(_, rules, default) = workflows(workflowName)
    val (unroutedOrthope, routings) = rules.foldLeft[(Orthope, List[(Orthope, WorkflowName)])]((orthope, List()))({
      case ((notYetRouted, routedSoFar), rule) =>
        val (yes, no) = split(notYetRouted, rule)
        (no, (yes, rule.destination) :: routedSoFar)
    })
    numAcceptable((unroutedOrthope, default) :: routings ::: rest, soFar)
}

val start: Orthope = "xmas".map(_ -> (1 to 4000)).toMap
numAcceptable(List((start, "in")))