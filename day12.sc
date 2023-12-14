import common.{loadPackets, time}

case class Problem(springs: String, broken: List[Int]) {
  def unfold: Problem = Problem(List.fill(5)(springs).mkString("?"), List.fill(5)(broken).flatten)
}

val input = loadPackets(List("day12.txt")).map {
  case s"${springs} ${broken}" => Problem(springs, broken.split(",").map(_.toInt).toList)
}

def memoize[K, V](f: K => V): K => V = {
  val cache = scala.collection.mutable.Map.empty[K, V]
  k => cache.getOrElseUpdate(k, f(k))
}

def memoizedOptions: Problem => Long = memoize({
  case Problem(springs, Nil) => if (springs.contains('#')) 0 else 1
  case Problem("", _) => 0
  case Problem(s"?${rest}", broken) => memoizedOptions(Problem(s".${rest}", broken)) +
    memoizedOptions(Problem(s"#${rest}", broken))
  case Problem(s".${rest}", broken) => memoizedOptions(Problem(rest, broken))
  case Problem(springs, broken) if springs.length >= broken.head && !springs.take(broken.head).contains('.') =>
    springs.drop(broken.head).headOption match {
      case Some('#') => 0
      case Some(_) => memoizedOptions(Problem(springs.drop(broken.head + 1), broken.tail))
      case _ => memoizedOptions(Problem(springs.drop(broken.head), broken.tail))
    }
  case _ => 0
})

val part1 = input.map(memoizedOptions).sum
time {
  memoizedOptions(input(3).unfold)
}
//val part2 = input.map(_.unfold).map(memoizedOptions).sum