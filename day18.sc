import common.loadPackets

val input = loadPackets(List("day18.txt"))

case class Point(row: Int, col: Int) {
  def move(direction: Char, amount: Int = 1): Point = direction match {
    case 'R' => copy(col = col + amount)
    case 'D' => copy(row = row + amount)
    case 'L' => copy(col = col - amount)
    case 'U' => copy(row = row - amount)
  }
}

case class Ditch(from: Point, to: Point, direction: Char) {
  def crossesBelowRow(row: Int): Boolean = direction match {
    case 'D' => (from.row until to.row).contains(row)
    case 'U' => (to.row until from.row).contains(row)
    case _ => false
  }
}

case class Instruction(direction: Char, amount: Int) {
  def move(from: Point): Point = from.move(direction, amount)
}

def contain(a: Range, b: Range): Boolean = a.start <= b.start && a.end >= b.end

def intersect(above: List[Range], below: List[Range]): List[Range] =
  (above ::: below).flatMap(range => List(range.start, range.end)).distinct.sorted.sliding(2)
    .toList.map { case List(a, b) => a until b }
    .filter(candidate => above.exists(a => contain(a, candidate)) && below.exists(b => contain(b, candidate)))

def computePoolSize(instructions: List[Instruction]): Long = {
  val points: List[Point] = instructions.scanLeft(Point(0, 0))((point, instruction) => instruction.move(point))
  val ditches: List[Ditch] = points.sliding(2).zip(instructions).map {
    case (List(from, to), Instruction(direction, _)) => Ditch(from, to, direction)
  }.toList

  def numHolesInRow(row: Int): Int = {
    // Check where the centers of vertical ditches cross the top and bottom lines of this row
    // The ranges between those crossings are the insides of the trenches.
    // Now combine these two lists of intervals to determine the columns on this row
    // that are going to get dug out.
    // These are the columns where the top and bottom ranges intersect.
    val intervalsAbove = ditches.filter(_.crossesBelowRow(row - 1)).map(_.from.col).sorted
      .grouped(2).map { case List(a, b) => a + 1 until b }.toList
    val intervalsBelow = ditches.filter(_.crossesBelowRow(row)).map(_.from.col).sorted
      .grouped(2).map { case List(a, b) => a + 1 until b }.toList
    intersect(intervalsAbove, intervalsBelow).map(_.length).sum
  }

  val trench = instructions.map(_.amount).sum
  val interestingRows = ditches.map(_.to.row).flatMap(x => x to x + 1).sorted.distinct
  val inners = interestingRows
    .map(row => (row, numHolesInRow(row))).sliding(2).toList
    .map { case List((from, num), (to, _)) => (from until to).length * num.toLong }
    .sum
  trench + inners
}

val part1 = computePoolSize(input.map {
  case s"${direction} ${amount} (#${_})" => Instruction(direction.head, amount.toInt)
})

val regex = """.*\(#(\w{5})(\w)\)""".r
val part2 = computePoolSize(input.map {
  case regex(hex, direction) => Instruction("RDLU"(direction.toInt), Integer.parseInt(hex, 16))
})