import common.{loadPackets, time}

import scala.collection.parallel.CollectionConverters.seqIsParallelizable

case class Problem(springs: String, broken: List[Int]) {
  lazy val firstBrokenForSure = springs.indexOf('#')
  lazy val undecided = springs.length - broken.sum - (broken.length - 1)

  def startingWith(working: Int): Option[Problem] = {
    if (!((working + broken.head <= springs.length) &&
      springs.substring(0, working).forall(_ != '#') &&
      springs.substring(working, working + broken.head).forall(_ != '.') &&
      (broken.tail.isEmpty || ((springs.length > working + broken.head) && springs.charAt(working + broken.head) != '#')))) None
    else
      Some(Problem(springs.substring(working + broken.head + (if(broken.length > 1) 1 else 0)), broken.tail))
  }

  lazy val noRoomForLeftoverBroken = springs.count(_ != '.') < broken.sum
  lazy val noRoomForLeftoverWorking = springs.count(_ != '#') < broken.length - 1
  lazy val notEnoughSpringsLeft = springs.count(_ == '#') > broken.sum
  lazy val impossible = noRoomForLeftoverBroken || noRoomForLeftoverWorking || notEnoughSpringsLeft || undecided < 0
  lazy val done = broken.isEmpty || impossible
  lazy val solved = broken.isEmpty && firstBrokenForSure == -1

  def unfold: Problem = Problem(List.fill(5)(springs).mkString("?"), List.fill(5)(broken).flatten)
}

val input = loadPackets(List("day12.txt")).map {
  case s"${springs} ${broken}" => Problem(springs, broken.split(",").map(_.toInt).toList)
}

def memoize[K, V](f: K => V): K => V = {
  val cache = scala.collection.mutable.Map.empty[K, V]
  k => cache.getOrElseUpdate(k, f(k))
}

def memoizedOptions: (Problem => Long) = memoize((problem) =>
  if (problem.done)
    if (problem.solved) 1 else 0
  else (0 to problem.undecided).par.flatMap(problem.startingWith)
    .map(memoizedOptions)
    .sum
)

val part1 = time {
  input.map(memoizedOptions).sum
}
time {
  memoizedOptions(input(3).unfold)
}
val part2 = time {
  input.map(_.unfold).map(memoizedOptions).sum
}
