val input = common.loadPackets(List("day25.txt"))

val connections = input.flatMap {
  case s"${from}: ${tos}" => tos.split("\\s+").map(_.trim).map(Set(from, _))
}

val nodes = connections.flatten

nodes.groupMapReduce(identity)(node => connections.count(_.contains(node)))(_+_)

// input for neato to determine which connections to cut
println(connections.map(_.toList).map{
  case a :: b :: Nil => s"${a} -- ${b}"
}.mkString("\n"))

val cut = Set(Set("ttv", "ztc"), Set("vfh", "bdj"), Set("rpd", "bnv"))

def combine(groups: List[Set[String]], connection: Set[String]): List[Set[String]] = {
  val (unconnected, connected) = groups.partition(group => group.intersect(connection).isEmpty)
  (connected.toSet.flatten ++ connection) :: unconnected
}

val part1 = connections.filterNot(cut).foldLeft(List[Set[String]]())(combine)
  .map(_.size).product