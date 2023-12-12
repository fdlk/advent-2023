import common.loadPackets

case class Problem(springs: String, broken: List[Int]) {
  val toDistribute = springs.length - broken.sum - (broken.length - 1)
  def startingWith(working: Int): Option[Problem] = {
    val toSubtract = List.fill(working)('.') ::: List.fill(broken.head)('#') ::: List('.').filter(_ => broken.length > 1)
    if (toSubtract.zip(springs).forall {
      case (_, '?') => true
      case (a, b) if a != b => false
      case _ => true
    }) Some(Problem(springs.substring(toSubtract.length), broken.tail))
    else None
  }
  val done = broken.isEmpty
  val solved = done && !springs.contains('#')

  def unfold: Problem = Problem(List(springs, springs, springs, springs, springs).mkString("?"), broken ::: broken ::: broken ::: broken ::: broken)
}

val input = loadPackets(List("day12.txt")).map{
  case s"${springs} ${broken}" => Problem(springs, broken.split(",").map(_.toInt).toList)
}

def options(problem: Problem): Long = {
  if (problem.done)
    if (problem.solved) 1 else 0
  else (0 to problem.toDistribute)
    .flatMap(problem.startingWith)
    .map(options)
    .sum
}

val part1 = input.map(options).sum

val part2 = input.map(_.unfold).map(options).sum
