import common.loadPackets

val input = loadPackets(List("day08.txt"))
val instructions = LazyList.continually(input.head.toList).flatten
val nodes = input.tail.tail.map{
  case s"${from} = (${left}, ${right})" => from -> (left, right)
}.toMap

val part1 = instructions.scanLeft("AAA")((node, instruction) => (nodes(node), instruction) match {
  case ((left, _), 'L') => left
  case ((_, right), 'R') => right
}).indexOf("ZZZ")