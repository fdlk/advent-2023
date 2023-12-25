import scala.annotation.tailrec
import scala.util.Random

val input = common.loadPackets(List("day25.txt"))

val connections = input.flatMap {
  case s"${from}: ${tos}" => tos.split("\\s+").map(_.trim).map(Set(from, _))
}

val nodes = connections.flatten

@tailrec
def findCuts(connections: List[Set[String]], groups: Set[Set[String]]): Int =
  if (groups.size == 2) {
    groups.toList.map(_.size).product
  } else {
    val (unconnected, connected) = groups.partition(group => group.intersect(connections.head).isEmpty)
    findCuts(connections.tail, unconnected + connected.flatten)
  }

val part1 = (1 to 50).map(_ => findCuts(Random.shuffle(connections), nodes.map(Set(_)).toSet)).max