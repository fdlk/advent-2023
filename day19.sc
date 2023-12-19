import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day19.txt"))

case class Part(x: Int, m: Int, a: Int, s: Int)
case class Rule(property: Char, comparison: Char, value: Int, routing: String) {
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
case class Workflow(name: String, rules: List[Rule], default: String) {
  def route(part: Part): String = rules.find(_.matches(part)).map(_.routing).getOrElse(default)
}

val regex = """([xmas])([<>])(.+):(.+)""".r
def parseRule(rule: String): Rule = rule match {
  case regex(property, comparison, value, routing) =>
    Rule(property.charAt(0), comparison.charAt(0), value.toInt, routing)
}

val workflowRegex = """(.+)\{(.*),([^,]+)}""".r
def parseWorkflow(line: String): (String, Workflow) = line match {
  case workflowRegex(name,rules,default) => name -> Workflow(name, rules.split(",").map(parseRule).toList, default)
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

val edges = workflows.values.flatMap(workflow => workflow.rules)
  .groupMapReduce[Char, List[Int]](_.property)(rule => List(rule.edge))((a, b) => a.concat(b).distinct.sorted)

def splitAt(ranges: List[Range], value: Int): List[Range] = {
  val (containing, notContaining) = ranges.partition(_.contains(value))
  (containing.flatMap(range => List(range.start until value, value until range.end)) ::: notContaining).sortBy(_.start)
}

val options = List(1 until 4001)
val intervals = edges.view.mapValues(_.foldLeft(options)(splitAt)).toMap

val part2 = (
  for(xs <- intervals('x');
    ms <- intervals('m');
    as <- intervals('a');
    ss <- intervals('s') if accept(Part(xs.start, ms.start, as.start, ss.start), "in"))
  yield xs.length.toLong * ms.length.toLong * as.length.toLong * ss.length.toLong).sum
