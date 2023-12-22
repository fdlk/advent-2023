import scala.annotation.tailrec

case class Point(x: Int, y: Int, z: Int) {
  lazy val isSupportedByGround: Boolean = z == 1
  lazy val pointBelow = copy(z = z - 1)
}

case class Brick(index: Int, points: Set[Point]) {
  lazy val below: Brick = copy(points = points.map(_.pointBelow))
  def supports(other: Point): Boolean = points.contains(other.pointBelow)
  def isSupportedBy(brick: Brick): Boolean = points.exists(point => brick.supports(point))
  def fall(others: List[Brick]): Brick =
    if (points.exists(_.isSupportedByGround) || others.exists(this.isSupportedBy))
      this
    else below
}

val bricks = common.loadPackets(List("day22.txt")).map({
  case s"${x1},${y1},${z1}~${x2},${y2},${z2}" =>
    if (x1 != x2)
      (x1.toInt to x2.toInt).map(x => Point(x, y1.toInt, z1.toInt))
    else if (y1 != y2)
      (y1.toInt to y2.toInt).map(y => Point(x1.toInt, y, z1.toInt))
    else
      (z1.toInt to z2.toInt).map(z => Point(x1.toInt, y1.toInt, z))
}).zipWithIndex.map { case (points, index) => Brick(index, points.toSet) }

@tailrec
def dropBricks(bricks: List[Brick]): List[Brick] = {
  val dropped = bricks.map(brick => brick.fall(bricks.filter(_ != brick)))
  if (dropped.toSet == bricks.toSet) {
    bricks
  } else dropBricks(dropped)
}

val dropped: List[Brick] = dropBricks(bricks)

case class Support(under: Int, upper: Int) {
  def concerns(bricks: Set[Int]): Boolean = bricks(under) || bricks(upper)
}

val supports: List[Support] = dropped.combinations(2)
  .flatMap { case List(a, b) => List((a, b), (b, a))}
  .filter { case (a, b) => b.isSupportedBy(a) }
  .map { case (under, upper) => Support(under.index, upper.index) }
  .toList

def canBeDisintegrated(brick: Int): Boolean = {
  val (us, them) = supports.partition(_.under == brick)
  us.map(_.upper).forall(upper => them.exists(_.upper == upper))
}

val part1 = dropped.map(_.index).count(canBeDisintegrated)

@tailrec
def impact(chainReaction: Set[Int], supports: List[Support] = supports): Int = {
  val (us, them) = supports.partition(_.concerns(chainReaction))
  val impacted = us.map(_.upper).filterNot(upper => them.exists(_.upper == upper))
  if (impacted.isEmpty)
    chainReaction.size - 1
  else
    impact(chainReaction ++ impacted, them)
}

val part2 = dropped.map(brick => impact(Set(brick.index))).sum