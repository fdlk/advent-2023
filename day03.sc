import common.loadPackets

val input = loadPackets(List("day03.txt"))

case class Point(row: Int, col: Int) {
  def isAdjacentTo(other: Point): Boolean = Math.abs(row - other.row) <= 1 && Math.abs(col - other.col) <= 1
}

case class Symbol(location: Point, char: Char) {
  def isGear: Boolean = char == '*'
  def isSymbol: Boolean = char != '.' && !char.isDigit
}

case class PartNumber(row: Int, cols: Range, number: Int) {
  def isAdjacentTo(symbol: Symbol): Boolean = cols.map(Point(row, _)).exists(symbol.location.isAdjacentTo)
}

val symbols = input.zipWithIndex.flatMap {
  case (line, row) => line.zipWithIndex.map {
    case (char, col) => Symbol(Point(row, col), char)
  }
}.filter(_.isSymbol)

val partNumbers: List[PartNumber] = input.zipWithIndex.flatMap {
  case (line, row) => """\d+""".r.findAllMatchIn(line)
    .map(m => PartNumber(row, m.start until m.end, m.group(0).toInt))
}.filter(partNumber => symbols.exists(partNumber.isAdjacentTo))

val part1 = partNumbers.map(_.number).sum

def gearRatio(gear: Symbol): Option[Int] =
  Some(partNumbers.filter(p => p.isAdjacentTo(gear)))
    .filter(_.length == 2)
    .map(_.map(_.number).product)

val part2 = symbols.filter(_.isGear).flatMap(gearRatio).sum