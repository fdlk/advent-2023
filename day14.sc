import common.loadPackets

import scala.annotation.tailrec

type Map = List[String]
val input: Map = loadPackets(List("day14.txt"))

case class Point(row: Int, col: Int) {
  def withNeighbors(direction: Char): List[Point] = direction match {
    case 'N' => List(copy(row = row - 1), this, copy(row = row + 1))
    case 'S' => List(copy(row = row + 1), this, copy(row = row - 1))
    case 'E' => List(copy(col = col + 1), this, copy(col = col - 1))
    case 'W' => List(copy(col = col - 1), this, copy(col = col + 1))
  }
}

def charOnMap(point: Point, map: Map): Option[Char] = point match {
  case Point(row, col) => if (map.indices.contains(row) && map(row).indices.contains(col))
    Some(map(row)(col))
  else
    None
}

def tilt(map: Map, direction: Char): Map = map.indices.map(row => map.head.indices.map(col =>
  Point(row, col).withNeighbors(direction).map(charOnMap(_, map)) match {
    case List(_, Some('.'), Some('O')) => 'O'
    case List(Some('.'), Some('O'), _) => '.'
    case List(_, Some(x), _) => x
  }).mkString).toList

@tailrec
def tilted(map: Map, direction: Char): Map = {
  val result = tilt(map, direction)
  if (result == map) result
  else tilted(result, direction)
}

def load(map: Map): Int = map.zipWithIndex.map {
  case (line, row) => line.count(_ == 'O') * (input.length - row)
}.sum

val part1 = load(tilted(input, 'N'))

def cycle(map: Map): Map = List('N', 'W', 'S', 'E').foldLeft(map)(tilted)
val loads = LazyList.iterate(input)(cycle).drop(300).map(load).take(50).toList

val part2 = loads((1000000000 - 300) % loads.indexOf(loads.head, 1))