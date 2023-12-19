import common.loadPackets

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

  def crossesAboveRow(row: Int): Boolean = crossesBelowRow(row - 1)
}

case class Instruction(direction: Char, amount: Int) {
  def move(from: Point): Point = from.move(direction, amount)
}

val regex = """.*\(#(\w{5})(\w)\)""".r

val instructions = loadPackets(List("day18.txt")).map {
  case regex(hex, direction) => Instruction("RDLU"(direction.toInt), Integer.parseInt(hex, 16))
}

val ditches: List[Ditch] = instructions.scanLeft[(Point, Option[Ditch])]((Point(0, 0), None)) {
  case ((from, _), instruction) => {
    val to = instruction.move(from)
    (to, Some(Ditch(from, to, instruction.direction)))
  }
}.tail.map(_._2.get)

def contain(a: Range, b: Range): Boolean = a.start <= b.start && a.end >= b.end

def combine(above: List[Range], below: List[Range]): List[Range] = {
  val result = (above, below) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (a :: restAbove, b :: restBelow) if a == b => a :: combine(restAbove, restBelow)
    case (a :: restAbove, b :: restBelow) if contain(a, b) => b :: combine((b.end until a.end) :: restAbove, restBelow)
    case (a :: restAbove, b :: restBelow) if contain(b, a) => a :: combine(restAbove, (a.end until b.end) :: restBelow)
    case (a :: restAbove, b :: _) if a.end <= b.start => combine(restAbove, below)
    case (a :: _, b :: restBelow) if b.end <= a.start => combine(above, restBelow)
    case (a :: restAbove, b :: restBelow) if a.start <= b.start =>
      (b.start until a.end) :: combine(restAbove, (a.end until b.end) :: restBelow)
    case (a :: restAbove, b :: restBelow) =>
      (a.start until b.end) :: combine((b.end until a.end) :: restAbove, restBelow)
  }
  result.filter(_.nonEmpty)
}

def numHolesInRow(row: Int) = {
  val intervalsAbove = ditches.filter(_.crossesAboveRow(row)).map(_.from.col).sorted
    .grouped(2).map { case List(a, b) => a + 1 until b }.toList
  val intervalsBelow = ditches.filter(_.crossesBelowRow(row)).map(_.from.col).sorted
    .grouped(2).map { case List(a, b) => a + 1 until b }.toList
  combine(intervalsAbove, intervalsBelow).map(_.length).sum
}

val trench = instructions.map(_.amount).sum

val inners = ditches.map(_.to.row).flatMap(x => x - 1 to x + 1).sorted.distinct
  .map(row => (row, numHolesInRow(row))).sliding(2).toList
  .map { case List((from, num), (to, _)) => (from until to).length * num.toLong }
  .sum

val part2 = trench + inners
