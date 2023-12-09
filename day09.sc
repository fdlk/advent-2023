import common.loadPackets

val input = loadPackets(List("day09.txt")).map(_.split("\\s+").map(_.toInt).toList)

def next(series: Seq[Int]): Int =
  if (series.forall(_ == 0)) 0
  else series.takeRight(1).head + next(series.sliding(2).map { case List(a, b) => b - a }.toSeq)

val part1 = input.map(next).sum