import common.{Grid, aStarSearch, loadPackets}

val map = loadPackets(List("day17.txt")).map(_.map(_ - '0'))

val rows = map.indices
val cols = map.head.indices

case class Point(row: Int, col: Int) {
  def isOnMap: Boolean = rows.contains(row) && cols.contains(col)

  def move(direction: Char): Option[Point] = Some(direction match {
    case 'N' => copy(row = row - 1)
    case 'S' => copy(row = row + 1)
    case 'E' => copy(col = col + 1)
    case 'W' => copy(col = col - 1)
  }).filter(_.isOnMap)

  def distanceTo(other: Point): Int = (other.row - row).abs + (other.col - col).abs

  def heatloss: Int = map(row)(col)
}

def opposite(direction: Char): Char = direction match {
  case 'N' => 'S'
  case 'S' => 'N'
  case 'E' => 'W'
  case 'W' => 'E'
}

case class State(location: Point, lastThreeMoves: List[Char]) {
  def move(direction: Char): Option[State] = location.move(direction)
    .map(point => copy(location = point, lastThreeMoves = direction :: lastThreeMoves.take(2)))

  def neighbors: Seq[State] = lastThreeMoves match {
    case List(a, b, c) if a == b && b == c => "NSEW".filter(_ != a).filter(_ != opposite(a)).flatMap(move)
    case lastMove :: _ => "NSEW".filter(_ != opposite(lastMove)).flatMap(move)
    case _ => "NSEW".flatMap(move)
  }
}

val finish = Point(rows.last, cols.last)

val grid: Grid[State] = new Grid[State] {

  override def heuristicDistanceToFinish(from: State): Int = from.location.distanceTo(finish)

  override def getNeighbours(state: State): Iterable[State] = state.neighbors

  override def moveCost(from: State, to: State): Int = to match {
    case State(Point(row, col), _) => map(row)(col)
  }
}

aStarSearch[State](State(Point(0, 0), Nil), grid, _.location == finish)