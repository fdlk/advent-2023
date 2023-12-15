import common.loadPackets

val input = loadPackets(List("day15.txt")).head.split(',')
def hashcode(step: String): Int =
  step.foldLeft(0)((current, ascii) => (current + ascii) * 17 % 256)

val part1 = input.map(hashcode).sum