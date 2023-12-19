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
}
case class Workflow(name: String, rules: List[Rule], default: String) {
  def route(part: Part): String = rules.find(_.matches(part)).map(_.routing).getOrElse(default)
}

val regex = """([xmas])([<>])(.+):(.+)""".r
def parseRule(rule: String): Rule = rule match {
  case regex(property, comparison, value, routing) =>
    Rule(property.charAt(0), comparison.charAt(0), value.toInt, routing)
}

val workflowRegex = """(.+)\{(.*),([^,]+)\}""".r
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

val part1 = parts.filter(accept(_, "in")).map(part => part.x + part.a + part.m + part.s).sum