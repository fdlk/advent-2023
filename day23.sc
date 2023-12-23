import common.loadPackets

import scala.annotation.tailrec

val input = loadPackets(List("day23.txt"))

val rows = input.indices
val cols = input.head.indices

case class Point(row: Int, col: Int) {
  lazy val isOnMap: Boolean = rows.contains(row) && cols.contains(col)
  lazy val glyph: Option[Char] = if (isOnMap) Some(input(row).charAt(col)) else None
  lazy val isPath: Boolean = isOnMap && !glyph.contains('#')
  def neighbors(): List[Point] = List(
      copy(row = row + 1),
      copy(col = col + 1),
      copy(row = row - 1),
      copy(col = col - 1)
    ).filter(_.isPath)
}

val start = cols.map(Point(0, _)).find(_.isPath).get
val finish = cols.map(Point(rows.last, _)).find(_.isPath).get

@tailrec
def followPath(from: Point, to: Point, steps: Int = 0): (Point, Int) = to.neighbors().filterNot(_ == from) match {
  case next :: Nil => followPath(to, next, steps + 1)
  case _ => (to, steps + 1)
}

val nodes: List[Point] = start :: finish :: (rows.toList.flatMap(row => cols.map(col => Point(row, col)))
  .filter(_.isPath).filter(_.neighbors().size > 2))

val routes: Map[Point, Map[Point, Int]] = nodes.map(node => node -> node.neighbors().filter(_.isPath)
  .map(to => followPath(node, to)).toMap).toMap

def findLongestHike(location: Point, visited: Set[Point], steps: Int): Option[Int] =
  if (location == finish) Some(steps)
  else {
    val hikes = routes(location).filterNot(route => visited(route._1))
      .flatMap({case (to, extraSteps) => findLongestHike(to, visited + to, steps + extraSteps)})
    if (hikes.isEmpty) None
    else Some(hikes.max)
  }
findLongestHike(start, Set(start), 0)
