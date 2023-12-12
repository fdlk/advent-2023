import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day10.txt"))

case class Point(row: Int, col: Int) {
  def move(direction: Char): Point = direction match {
    case 'N' => copy(row = row - 1)
    case 'S' => copy(row = row + 1)
    case 'E' => copy(col = col + 1)
    case 'W' => copy(col = col - 1)
  }

  def relativeTo(other: Point): Option[Char] = other match {
    case Point(r, c) if r == row && c > col => Some('E')
    case Point(r, _) if r == row => Some('W')
    case Point(r, c) if c == col && r < row => Some('N')
    case Point(_, c) if c == col => Some('S')
    case _ => None
  }
}

val maze = input.zipWithIndex.flatMap { case (line, row) =>
  line.zipWithIndex.map {
    case (char, col) => Point(row, col) -> char
  }
}.toMap

val pipes = Map('|' -> "NS", '-' -> "EW", 'L' -> "NE", 'J' -> "NW", '7' -> "SW", 'F' -> "SE")
val opposite = Map('N' -> 'S', 'E' -> 'W', 'W' -> 'E', 'S' -> 'N')
val startingDirection = 'S'

case class State(pos: Point, enteringFrom: Char) {
  val pipe: Char = maze(pos)
  lazy val direction: Char = if (pipe == 'S') startingDirection else pipes(pipe).filter(_ != enteringFrom).charAt(0)
  def next(): State = State(pos.move(direction), opposite(direction))
}

val start = maze.find(_._2 == 'S').get._1

val loop = start ::
  LazyList.iterate(State(start.move(startingDirection), opposite(startingDirection)))(_.next())
    .map(_.pos).takeWhile(_ != start).toList

val part1 = loop.size / 2

// as the loop turns around a point on the inside, it will have to pass to the north, east, west, and south
// so let's determine the directions in which the loop passes straight past the point and then simplify
// repetitions: SSSS => S and cases where it backtracks: SWS => S, SES => S
def points(p: Point): String = loop.flatMap(p.relativeTo).mkString
@tailrec
def simplify(s: String): String = {
  val dedupe = s.replaceAll("(.).?\\1", "$1")
  if (dedupe == s) dedupe else simplify(dedupe)
}

val part2 = input.indices.flatMap(row => input.head.indices.map(col => Point(row, col)))
  .filter(!loop.contains(_))
  .map(points)
  .map(simplify)
  .count(_.length >= 4)