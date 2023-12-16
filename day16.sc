import common.loadPackets
import scala.annotation.tailrec

val input = loadPackets(List("day16.txt"))

val rows = input.indices
val cols = input.head.indices

case class Point(row: Int, col: Int) {
  def isOnMap: Boolean = rows.contains(row) && cols.contains(col)

  def move(direction: Char): Point = direction match {
    case 'W' => copy(col = col - 1)
    case 'E' => copy(col = col + 1)
    case 'N' => copy(row = row - 1)
    case 'S' => copy(row = row + 1)
  }

  def moveOnMap(direction: Char): Option[Point] = Some(move(direction)).filter(_.isOnMap)

  def mirror: Char  = input(row)(col)
}

case class Beam(point: Point, direction: Char) {
  def reflect: Set[Char] = point.mirror match {
    case '.' => Set(direction)
    case '|' if "EW".contains(direction) => "NS".toSet
    case '|' => Set(direction)
    case '-' if "NS".contains(direction) => "EW".toSet
    case '-' => Set(direction)
    case '\\' => Set(Map('N' -> 'W', 'S' -> 'E', 'E' -> 'S', 'W' -> 'N')(direction))
    case '/' => Set(Map('N' -> 'E', 'S' -> 'W', 'E' -> 'N', 'W' -> 'S')(direction))
  }

  def next: Set[Beam] = reflect.flatMap(d => point.moveOnMap(d).map(Beam(_, d)))
}

@tailrec
def numEnergized(beams: Set[Beam], knownBeams: Set[Beam] = Set()): Int = {
  val newBeams = beams.flatMap(_.next).filter(!knownBeams(_))
  if (newBeams.isEmpty)
    knownBeams.map(_.point).size
  else
    numEnergized(newBeams, knownBeams ++ newBeams)
}

val start = Beam(Point(0, 0), 'E')
val part1 = numEnergized(Set(start), Set(start))

val beams = rows.map(row => Beam(Point(row, 0), 'E')) ++
  rows.map(row => Beam(Point(row, cols.last), 'W')) ++
  cols.map(col => Beam(Point(0, col), 'S')) ++
  cols.map(col => Beam(Point(rows.last, col), 'N'))

val part2 = beams.map(beam => numEnergized(Set(beam), Set(beam))).max
