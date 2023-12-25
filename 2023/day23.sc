import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day23.txt"))

val rows = input.indices
val cols = input.head.indices

case class Point(row: Int, col: Int) {
  lazy val isOnMap: Boolean = rows.contains(row) && cols.contains(col)
  lazy val glyph: Char = input(row).charAt(col)
  lazy val isPath: Boolean = isOnMap && glyph != '#'

  def neighbors(): List[Point] = List(
    copy(row = row + 1),
    copy(col = col + 1),
    copy(row = row - 1),
    copy(col = col - 1)
  ).filter(_.isPath)

  def neighborsSlippery(): List[Point] = (glyph match {
    case '.' => neighbors()
    case '<' => List(copy(col = col - 1))
    case '>' => List(copy(col = col + 1))
    case 'v' => List(copy(row = row + 1))
    case '^' => List(copy(row = row - 1))
  }).filter(_.isPath)
}

val start = cols.map(Point(0, _)).find(_.isPath).get
val finish = cols.map(Point(rows.last, _)).find(_.isPath).get

def findLongestHikeSlippery(location: Point, visited: Set[Point]): Option[Int] =
  if (location == finish)
    Some(visited.size - 1)
  else
    location.neighborsSlippery().filterNot(visited)
      .flatMap(next => findLongestHikeSlippery(next, visited + next))
      .maxOption

val part1 = findLongestHikeSlippery(start, Set(start))

val nodes: List[Point] = start :: finish :: rows.toList.flatMap(row => cols.map(col => Point(row, col)))
  .filter(_.isPath).filter(_.neighbors().size > 2)

@tailrec
def followPath(from: Point, to: Point, steps: Int = 0): (Point, Int) = to.neighbors().filterNot(_ == from) match {
  case next :: Nil => followPath(to, next, steps + 1)
  case _ => (to, steps + 1)
}

val routes: Map[Point, Map[Point, Int]] = nodes.map(node => node ->
  node.neighbors().filter(_.isPath).map(to => followPath(node, to)).toMap).toMap

def findLongestHike(location: Point, visited: Set[Point], steps: Int = 0): Option[Int] =
  if (location == finish)
    Some(steps)
  else
    routes(location).filterNot(route => visited(route._1))
      .flatMap({ case (to, extraSteps) => findLongestHike(to, visited + to, steps + extraSteps) })
      .maxOption

val part2 = findLongestHike(start, Set(start))