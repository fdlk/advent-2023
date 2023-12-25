import common.loadPackets

val input = loadPackets(List("day21.txt"))

val rows = input.indices
val cols = input.head.indices
val whole = rows.length

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

def reachable(from: Set[Point]): Set[Point] = from.flatMap(_.neighbors()).filter(!rocks(_))

def iterations(start: Point): LazyList[Set[Point]] = LazyList.iterate(Set(start))(reachable(_).filter(_.isOnMap))

val part1 = LazyList.iterate(Set(start))(reachable)(64).size

val steps = 26501365
val half = steps % whole
val topLeft = Point(0, 0)
val middleLeft = Point(half, 0)
val bottomLeft = Point(rows.last, 0)
val middleBottom = Point(rows.last, half)
val bottomRight = Point(rows.last, cols.last)
val middleRight = Point(half, cols.last)
val topRight = Point(0, cols.last)
val middleTop = Point(0, half)

val wholeOdd = iterations(start)(2 * whole + 1).size
val wholeEven = iterations(start)(2 * whole).size
val threeQuarterTopRight = iterations(bottomLeft)(whole + half-1).size
val threeQuarterBottomRight = iterations(topLeft)(whole + half-1).size
val threeQuarterBottomLeft = iterations(topRight)(whole + half-1).size
val threeQuarterTopLeft = iterations(bottomRight)(whole + half-1).size
val oneQuarterTopRight = iterations(topRight)(half-1).size
val oneQuarterBottomRight = iterations(bottomRight)(half-1).size
val oneQuarterBottomLeft = iterations(bottomLeft)(half-1).size
val oneQuarterTopLeft = iterations(topLeft)(half-1).size
val spearheadTop = iterations(middleBottom)(whole-1).size
val spearheadRight = iterations(middleLeft)(whole-1).size
val spearheadBottom = iterations(middleTop)(whole-1).size
val spearheadLeft = iterations(middleRight)(whole-1).size

val n = steps / 131

val cubes: List[(Long, Long)] = List((wholeEven, (1 to n).map(_.toLong).sum + (1 until n).map(_.toLong).sum),
  (wholeOdd, (1 until n).map(_.toLong).sum + (1 until n-1).map(_.toLong).sum),
  (threeQuarterTopRight, n-1),
  (threeQuarterBottomRight, n-1),
  (threeQuarterBottomLeft, n-1),
  (threeQuarterTopLeft, n-1),
  (oneQuarterTopRight, n),
  (oneQuarterBottomRight, n),
  (oneQuarterBottomLeft, n),
  (oneQuarterTopLeft, n),
  (spearheadTop, 1),
  (spearheadRight, 1),
  (spearheadBottom, 1),
  (spearheadLeft, 1),
)

val part2 = cubes.map{ case (reached, count) => reached * count }.sum
