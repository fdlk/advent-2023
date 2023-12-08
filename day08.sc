import common.{loadPackets, lcm}

val input = loadPackets(List("day08.txt"))
val instructions = LazyList.continually(input.head.toList).flatten
val nodes = input.tail.tail.map {
  case s"${from} = (${left}, ${right})" => from -> (left, right)
}.toMap

def next(node: String, instruction: Char): String = (nodes(node), instruction) match {
  case ((left, _), 'L') => left
  case ((_, right), 'R') => right
}

val part1 = instructions.scanLeft("AAA")((node, instruction) => next(node, instruction)).indexOf("ZZZ")

def stepsToZ(from: String): Long =
  instructions.scanLeft(from)((node, instruction) => next(node, instruction)).indexWhere(_.takeRight(1) == "Z")

val part2 = lcm(nodes.keys.filter(_.takeRight(1) == "A").map(stepsToZ).toSeq)
