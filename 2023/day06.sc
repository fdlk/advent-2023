import common.loadPackets

val input = loadPackets(List("day06.txt"))

val parsed: List[(Int, Int)] = input.map(_.split(':')(1))
  .map(_.trim.split("\\s+").map(_.toInt).toList) match {
  case List(times, distances) => times.zip(distances)
}

def winningOptions(time: Int, distance: Int): Int =
  (0 to time)
    .map(velocity => (time - velocity) * velocity)
    .count(_ > distance)

val part1 = parsed.map {
  case (time, distance) => winningOptions(time, distance)
}.product

val parsedWithKerning: List[Long] = input.map(_.toList.filter(_.isDigit).mkString("").toLong)

def winningOptionsImpreciseButFast(time: Long, distance: Long): Double =
  Math.sqrt(time * time - 4 * distance)

val part2 = parsedWithKerning match {
  case List(time, distance) => winningOptionsImpreciseButFast(time, distance).round
}