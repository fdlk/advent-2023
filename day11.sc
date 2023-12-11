import common.loadPackets

val input = loadPackets(List("day11.txt"))

case class Galaxy(row: Int, col: Int)

val galaxies: List[Galaxy] = input.zipWithIndex.flatMap {
  case (line, row) => line.zipWithIndex.flatMap({
    case ('#', col) => Some(Galaxy(row, col))
    case _ => None
  })
}

val emptyCols = input.head.indices.filter(col => !galaxies.exists(_.col == col)).toSet
val emptyRows = input.indices.filter(row => !galaxies.exists(_.row == row)).toSet

def count(set: Set[Int], a: Int, b: Int) = set.intersect((a.min(b) to a.max(b)).toSet).size

def distance(from: Galaxy, to: Galaxy, expansion: Long): Long =
  (from.col - to.col).abs + count(emptyCols, from.col, to.col) * (expansion - 1) +
    (from.row - to.row).abs + count(emptyRows, from.row, to.row) * (expansion - 1)

val part1 = galaxies.combinations(2).map { case List(a, b) => distance(a, b, 2L) }.sum
val part2 = galaxies.combinations(2).map { case List(a, b) => distance(a, b, 1_000_000L) }.sum
