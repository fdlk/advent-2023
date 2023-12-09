import common.loadPackets

val input: List[Seq[Int]] = loadPackets(List("day09.txt")).map(_.split("\\s+").map(_.toInt))

val diffs: List[LazyList[Seq[Int]]] = input.map(
  LazyList.iterate(_)(_.sliding(2).map { case Seq(a, b) => b - a }.toSeq)
    .takeWhile(_.exists(_ != 0))
)

val part1 = diffs.flatMap(_.map(_.last)).sum
val part2 = diffs.map(_.map(_.head).reduceRight(_ - _)).sum