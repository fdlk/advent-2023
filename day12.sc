import common.loadPackets

case class Problem(springs: String, broken: List[Int]) {
  lazy val undecided = springs.length - broken.sum - (broken.length - 1)

  def startingWith(working: Int): Option[Problem] = {
    if (!((working + broken.head <= springs.length) &&
      springs.substring(0, working).forall(_ != '#') &&
      springs.substring(working, working + broken.head).forall(_ != '.') &&
      (broken.tail.isEmpty || ((springs.length > working + broken.head) && springs.charAt(working + broken.head) != '#')))) None
    else
      Some(Problem(springs.substring(working + broken.head + (if (broken.length > 1) 1 else 0)), broken.tail))
  }

  lazy val done = broken.isEmpty
  lazy val solved = broken.isEmpty && !springs.contains('#')

  def unfold: Problem = Problem(List.fill(5)(springs).mkString("?"), List.fill(5)(broken).flatten)

  def sequenceFits(numBroken: Int, pos: Int): Boolean = {
    // fit ".###." starting at pos
    springs(pos) != '#' && !(pos + 1 to pos + numBroken).map(springs).contains('.') && springs(pos + numBroken + 1) != '#'
  }

  def split: Seq[(Problem, Problem)] = {
    val splitBrokenAt = broken.length / 2
    val numBroken = broken(splitBrokenAt)
    val leftBroken = broken.take(splitBrokenAt)
    val rightBroken = broken.drop(splitBrokenAt + 1)
    val widthLeft = leftBroken.sum + leftBroken.length - 1
    val widthRight = rightBroken.sum + rightBroken.length - 1
    (widthLeft until (springs.length - widthRight - numBroken - 1))
      .filter(pos => sequenceFits(numBroken, pos))
      .map(pos => (Problem(springs.take(pos), leftBroken), Problem(springs.drop(pos + numBroken + 2), rightBroken)))
  }
}

val input = loadPackets(List("day12.txt")).map {
  case s"${springs} ${broken}" => Problem(springs, broken.split(",").map(_.toInt).toList)
}

def memoize[K, V](f: K => V): K => V = {
  val cache = scala.collection.mutable.Map.empty[K, V]
  k => cache.getOrElseUpdate(k, f(k))
}

def memoizedOptions: Problem => Long = memoize(problem =>
  if (problem.done)
    if (problem.solved) 1 else 0
  else if (problem.broken.length > 2)
    problem.split.map {
      case (left, right) => memoizedOptions(left) * memoizedOptions(right)
    }.sum
  else (0 to problem.undecided).flatMap(problem.startingWith)
    .map(memoizedOptions)
    .sum
)

val part1 = input.map(memoizedOptions).sum
val part2 = input.map(_.unfold).map(memoizedOptions).sum