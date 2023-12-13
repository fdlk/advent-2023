import common.loadPackets

val input = loadPackets(List("day13.txt"))

def chunks(s: List[String]): List[List[String]] = {
  val (h, t) = s.span(_.nonEmpty)
  if (h.isEmpty) Nil else h :: chunks(t drop 1)
}

def isMirrorRightAfter(line: String, index: Int): Boolean =
  line.substring(0, index + 1).reverse
    .zip(line.substring(index + 1))
    .forall { case (a, b) => a == b }

case class Maze(lines: List[String]) {
  def getColumn(col: Int): String =
    lines.indices.map(row => lines(row).charAt(col)).mkString

  def isVerticalMirrorRightAfterColumn(col: Int): Boolean =
    lines.forall(isMirrorRightAfter(_, col))

  def isHorizontalMirrorRightAfterRow(row: Int): Boolean =
    lines.head.indices.map(getColumn).forall(isMirrorRightAfter(_, row))

  def scores: Seq[Int] =
    lines.indices.dropRight(1)
      .filter(isHorizontalMirrorRightAfterRow)
      .map(row => (row + 1) * 100) ++
      lines.head.indices.dropRight(1).filter(isVerticalMirrorRightAfterColumn).map(col => col + 1)

  def smudgedAt(row: Int, col: Int): Maze = Maze(
    lines.zipWithIndex.map {
      case (line, r) if r == row => line.zipWithIndex.map {
        case ('#', c) if c == col => '.'
        case ('.', c) if c == col => '#'
        case (char, _) => char
      }.mkString
      case (line, _) => line
    })

  def smudged: List[Maze] = lines.indices.flatMap(row =>
    lines.head.indices.map(col => smudgedAt(row, col))).toList

  def smudgedScore = smudged.flatMap(_.scores).filter(_ != scores.head).head
}

val mazes: List[Maze] = chunks(input).map(Maze)

val part1 = mazes.map(_.scores.head).sum

val part2 = mazes.map(_.smudgedScore).sum