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

val pipes = Map(
  '|' -> "NS",
  '-' -> "EW",
  'L' -> "NE",
  'J' -> "NW",
  '7' -> "SW",
  'F' -> "SE"
)

val opposite = Map('N' -> 'S', 'E' -> 'W', 'W' -> 'E', 'S' -> 'N')


case class State(pos: Point, enteringFrom: Char) {
  def next(): State = {
    val pipe: Char = maze(pos)
    val direction = pipes(pipe).filter(_ != enteringFrom).charAt(0)
    val moved = pos.move(direction)
    State(moved, opposite(direction))
  }
}

val start = maze.find(_._2 == 'S').get._1

LazyList.iterate(State(start.move('S'), 'N'))(_.next())
  .takeWhile(_.pos != start)
  .toList
  .length / 2 + 1
