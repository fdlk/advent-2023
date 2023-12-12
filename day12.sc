import common.loadPackets
import scala.collection.parallel.CollectionConverters.seqIsParallelizable

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

  lazy val noRoomForLeftoverBroken = springs.count(_ != '.') < broken.sum
  lazy val noRoomForLeftoverWorking = springs.count(_ != '#') < broken.length - 1
  lazy val notEnoughSpringsLeft = springs.count(_ == '#') > broken.sum
  lazy val impossible = noRoomForLeftoverBroken || noRoomForLeftoverWorking || notEnoughSpringsLeft || toDistribute < 0
  lazy val done = broken.isEmpty || impossible
  lazy val solved = broken.isEmpty && !springs.contains('#')

  def unfold: Problem = {
    println("*")
    Problem(List(springs, springs, springs, springs, springs).mkString("?"), broken ::: broken ::: broken ::: broken ::: broken)
  }
}

val input = loadPackets(List("day12.txt")).map {
  case s"${springs} ${broken}" => Problem(springs, broken.split(",").map(_.toInt).toList)
}

def stillSolvable(problem: Problem): Boolean = {
  if (problem.done)
    problem.solved
  else (0 to problem.toDistribute).par
    .flatMap(problem.startingWith)
    .exists(stillSolvable)
}

def options(problem: Problem): Long = {
  if (problem.done)
    if (problem.solved) 1 else 0
  else if (true)
    (0 to problem.toDistribute).flatMap(problem.startingWith)
      .map(options)
      .sum
  else 0
}


val part1 = input.map(options).sum

options(input(3).unfold)
//151047661L
