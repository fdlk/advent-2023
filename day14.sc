import common.loadPackets

import scala.annotation.tailrec

type Map = List[String]
val input: Map = loadPackets(List("day14.txt"))

def charOnMap(row: Int, col: Int, map: Map): Option[Char] =
  if (map.indices.contains(row) && map(row).indices.contains(col))
    Some(map(row)(col))
  else
    None


def tilt(map: Map): Map = map.indices.map(row => map.head.indices.map(col =>
  (charOnMap(row - 1, col, map), charOnMap(row, col, map).get, charOnMap(row + 1, col, map)) match {
    case (_, '.', Some('O')) => 'O'
    case (Some('.'), 'O', _) => '.'
    case (_, x, _) => x
  }).mkString).toList

@tailrec
def tilted(map: Map): Map = {
  val result = tilt(map)
  if(result == map) result
  else tilted(result)
}

val part1 = tilted(input).zipWithIndex.map{
  case (line, row) => line.count(_ == 'O') * (input.length - row)
}.sum