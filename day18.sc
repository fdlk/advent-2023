import common.loadPackets

import scala.annotation.tailrec

case class Instruction(direction: Char, amount: Int, color: String)

val input = loadPackets(List("day18.txt")).map{
  case s"${direction} ${amount} (#${color})" => Instruction(direction.head, amount.toInt, color)
}

case class Point(row: Int, col: Int) {
  def move(direction: Char): Point = direction match {
    case 'R' => copy(col = col + 1)
    case 'D' => copy(row = row + 1)
    case 'L' => copy(col = col - 1)
    case 'U' => copy(row = row - 1)
  }

  def neighbors: Set[Point] = Set('R', 'D', 'L', 'U').map(move)
}

val points = input.flatMap(instruction => List.fill(instruction.amount)(instruction.direction))
  .scanLeft(Point(0, 0))((point, direction) => point.move(direction)).toSet

val rows = points.map(_.row).toList.sorted
val cols = points.map(_.col).toList.sorted

val pointInside = cols.map(col => Point(rows(1), col)).dropWhile(!points(_)).find(!points(_)).get

//val map = rows.map(row =>
//  row + cols.map(col =>Point(row, col))
//    .map{
//      case point if point == pointInside => '*'
//      case point if points(point) => '#'
//      case _ => '.'
//    }.mkString
//).mkString("\n")
//
//println(map)


@tailrec
def floodFill(open: Set[Point], inside: Set[Point] = points): Int =
  if (open.isEmpty)
    inside.size
  else {
    val known = inside ++ open
    val found = open.flatMap(_.neighbors).filter(!known(_))
    floodFill(found, known)
  }

floodFill(Set(pointInside))