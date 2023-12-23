import common.loadPackets

val input = loadPackets(List("day23.txt"))

val rows = input.indices
val cols = input.head.indices

case class Point(row: Int, col: Int) {
  lazy val isOnMap: Boolean = rows.contains(row) && cols.contains(col)
  lazy val glyph: Option[Char] =
    if (isOnMap) Some(input(row).charAt(col)) else None
  lazy val isPath: Boolean = isOnMap && !glyph.contains('#')

  def neighbors(): List[Point] = (glyph.get match {
    case '.' => List(
      copy(row = row + 1),
      copy(col = col + 1),
      copy(row = row - 1),
      copy(col = col - 1)
    )
    case '<' => List(copy(col = col - 1))
    case '>' => List(copy(col = col + 1))
    case 'v' => List(copy(row = row + 1))
    case '^' => List(copy(row = row - 1))
  }).filter(_.isPath)
}

val start = cols.map(Point(0, _)).find(_.isPath).get
val finish = cols.map(Point(rows.last, _)).find(_.isPath).get

def findLongestHike(location: Point, visited: Set[Point]): Option[Int] =
  if (location == finish) Some(visited.size)
  else {
    val hikes = location.neighbors().filterNot(visited)
      .flatMap(next => findLongestHike(next, visited + next))
    if (hikes.isEmpty) None
    else Some(hikes.max)
  }

findLongestHike(start, Set(start)).map(_ - 1)