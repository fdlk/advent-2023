import common.loadPackets

val input = loadPackets(List("day21.txt"))

val rows = input.indices
val cols = input.head.indices

case class Point(row: Int, col: Int) {
  val isOnMap: Boolean = rows.contains(row) && cols.contains(col)

  def neighbors(): List[Point] = List(
    copy(row = row + 1),
    copy(row = row - 1),
    copy(col = col + 1),
    copy(col = col - 1)
  )
}

val rocks: Set[Point] = rows.flatMap(row =>
  cols.filter(col => input(row)(col) == '#')
    .map(col => Point(row, col))).toSet

val start: Point = rows.flatMap(row =>
  cols.filter(col => input(row)(col) == 'S').map(col => Point(row, col))).head

def reachable(from: Set[Point]): Set[Point] =
  from.flatMap(_.neighbors()).filter(_.isOnMap).filter(!rocks(_))

LazyList.iterate(Set(start))(reachable)(64).size