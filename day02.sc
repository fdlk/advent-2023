import common.loadPackets

val input = loadPackets(List("day02.txt"))

val gameIdRegex = """Game (\d+): .*""".r
val cubeCountRegex = """(\d+) (blue|green|red)""".r

def maxSeen(line: String): Map[String, Int] =
  cubeCountRegex.findAllMatchIn(line).toList
    .groupMapReduce(_.group(2))(_.group(1).toInt)(Math.max)
    .withDefault(_ => 0)

def possible(line: String): Boolean = {
  val seen = maxSeen(line)
  seen("red") <= 12 && seen("green") <= 13 && seen("blue") <= 14
}

val part1 = input.filter(possible).map({ case gameIdRegex(id) => id.toInt }).sum

def power(line: String): Int = {
  val seen = maxSeen(line)
  seen("red") * seen("green") * seen("blue")
}

val part2 = input.map(power).sum