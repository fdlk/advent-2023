import common.loadPackets

val input = loadPackets(List("day10.txt"))

case class Point(row: Int, col: Int) {
  def move(direction: Char): Point = direction match {
    case 'N' => copy(row = row - 1)
    case 'S' => copy(row = row + 1)
    case 'E' => copy(col = col + 1)
    case 'W' => copy(col = col - 1)
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
val startingEnteringFrom = 'N'

case class State(pos: Point, enteringFrom: Char) {
  val pipe: Char = maze(pos)
  lazy val direction: Char = if (pipe == 'S') startingDirection else pipes(pipe).filter(_ != enteringFrom).charAt(0)
  val movingNorth: Boolean = enteringFrom == 'S' || direction == 'N'
  def next(): State = State(pos.move(direction), opposite(direction))
}

val start = maze.find(_._2 == 'S').get._1

val loop: Map[Point, State] = (State(start, startingEnteringFrom) ::
  LazyList.iterate(State(start.move(startingDirection), opposite(startingDirection)))(_.next())
  .takeWhile(_.pos != start)
  .toList).map(state => state.pos -> state).toMap

val part1 = loop.size / 2

def moveEastToLoop(p: Point): Option[State] =
  if (p.col == input.head.length) None
  else loop.get(p).orElse(moveEastToLoop(p.move('E')))

val part2 =
  input.indices.flatMap(row => input.head.indices.map(col => Point(row, col)))
    .filter(!loop.keySet(_))
    .flatMap(moveEastToLoop)
    .count(_.movingNorth)
