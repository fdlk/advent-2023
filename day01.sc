import common.loadPackets
import scala.util.matching.Regex

val input = loadPackets(List("day01.txt"))

def parse(value: String): Int = value match {
  case "one" => 1
  case "two" => 2
  case "three" => 3
  case "four" => 4
  case "five" => 5
  case "six" => 6
  case "seven" => 7
  case "eight" => 8
  case "nine" => 9
  case d => d.charAt(0) - '0'
}
def firstDigit(line: String, regex: Regex): Option[Int] = regex.findFirstIn(line).map(parse)
def lastDigit(line: String, regex: Regex):Int = line.indices.map(line.substring).flatMap(firstDigit(_, regex)).last
def calibrationValue(line: String, regex: Regex): Int = 10 * firstDigit(line, regex).get + lastDigit(line, regex)

val part1 = input.map(calibrationValue(_, """\d""".r)).sum
val part2 = input.map(calibrationValue(_, """one|two|three|four|five|six|seven|eight|nine|\d""".r)).sum