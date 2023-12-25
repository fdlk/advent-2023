import common.{Grid, aStarSearch, loadPackets, time}

val map = loadPackets(List("day17.txt")).map(_.map(_ - '0'))

val rows = map.indices
val cols = map.head.indices

case class Point(row: Int, col: Int) {
  lazy val isOnMap: Boolean = rows.contains(row) && cols.contains(col)

  def move(direction: Char): Option[Point] = Some(direction match {
    case 'N' => copy(row = row - 1)
    case 'S' => copy(row = row + 1)
    case 'E' => copy(col = col + 1)
    case 'W' => copy(col = col - 1)
  }).filter(_.isOnMap)

  def distanceTo(other: Point): Int = (other.row - row).abs + (other.col - col).abs

  def heatloss: Int = map(row)(col)
}

val finish = Point(rows.last, cols.last)

def opposite(direction: Char): Char = direction match {
  case 'N' => 'S'
  case 'S' => 'N'
  case 'E' => 'W'
  case 'W' => 'E'
}

case class State(location: Point, lastMove: Char, speed: Int) {
  lazy val canTurnOrStop: Boolean = speed >= 4
  lazy val mustTurn: Boolean = speed == 10
  lazy val isFinished: Boolean = location == finish && canTurnOrStop

  def move(direction: Char): Option[State] = location.move(direction)
    .map(point => copy(location = point, lastMove = direction, speed = if(direction == lastMove) speed + 1 else 1))

  def neighbors: Seq[State] =
    if (mustTurn)
      "NSEW".filter(_ != lastMove).filter(_ != opposite(lastMove)).flatMap(move)
    else if (canTurnOrStop)
      "NSEW".filter(_ != opposite(lastMove)).flatMap(move)
    else if (lastMove != 'X')
      move(lastMove).toList
    else
      "NSEW".flatMap(move)
}

val grid: Grid[State] = new Grid[State] {
  override def heuristicDistanceToFinish(from: State): Int = finish.distanceTo(from.location)
  override def getNeighbours(state: State): Iterable[State] = state.neighbors
  override def moveCost(from: State, to: State): Int = to.location.heatloss
}

time {
  aStarSearch[State](State(Point(0, 0), 'X', 0), grid, _.isFinished)
}