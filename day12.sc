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

  lazy val done = broken.isEmpty
  lazy val solved = broken.isEmpty && firstBrokenForSure == -1

  def unfold: Problem = Problem(List.fill(5)(springs).mkString("?"), List.fill(5)(broken).flatten)

  val canSplit = springs.length > 10 && springs.indexOf('.', 1) > 1 && springs.indexOf('.', 1) < springs.length - 1

  def split: Seq[(Problem, Problem)] = {
    val possibleSplitPoints = springs.indices.filter(springs.charAt(_) == '.')
    val at = possibleSplitPoints(possibleSplitPoints.length / 2)
    val left = springs.substring(0, at)
    val right = springs.substring(at)
    (0 to broken.length).map(numLeftBroken =>
      (Problem(left, broken.take(numLeftBroken)), Problem(right, broken.drop(numLeftBroken))))
  }
}

val input = loadPackets(List("day12.txt")).map {
  case s"${springs} ${broken}" => Problem(springs, broken.split(",").map(_.toInt).toList)
}

def memoize[K, V](f: K => V): K => V = {
  val cache = scala.collection.mutable.Map.empty[K, V]
  k => cache.getOrElseUpdate(k, f(k))
}

def memoizedOptions: (Problem => Long) = {
  def options(problem: Problem): Long = if (problem.done)
    if (problem.solved) 1 else 0
  else if (problem.canSplit)
    problem.split.map {
      case (left, right) => memoizedOptions(left) * memoizedOptions(right)
    }.sum
  else {
    (0 to problem.undecided).flatMap(problem.startingWith)
      .map(memoizedOptions)
      .sum
  }
  memoize(options)
}

val part1 = time {
  input.map(memoizedOptions).sum
}
time {
  memoizedOptions(input(3).unfold)
}
val part2 = time {
  input.map(_.unfold).par.map(memoizedOptions).sum
}
