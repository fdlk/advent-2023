val input = common.loadPackets(List("day24.txt"))

case class Point(x: Long, y: Long, z: Long)

case class Hailstone(pos: Point, velocity: Point) {
  val a: Float = velocity.y.toFloat / velocity.x
  val b: Float = pos.y - a * pos.x

  def isInFuture(x: Float): Boolean = pos.x < x && velocity.x > 0 ||
    pos.x > x && velocity.x < 0

  def intersect(other: Hailstone): Option[(Float, Float)] = {
    if (a == other.a)
      None
    else {
      val xs = (other.b - b) / (a - other.a)
      val ys = a * xs + b
      if (isInFuture(xs) && other.isInFuture(xs))
        Some((xs, ys))
      else None
    }
  }
}

val hailstones = input.map {
  case s"${x}, ${y}, ${z} @ ${vx}, ${vy}, ${vz}" =>
    Hailstone(Point(x.trim.toLong, y.trim.toLong, z.trim.toLong),
      Point(vx.trim.toInt, vy.trim.toInt, vz.trim.toInt))
}

val min = 200000000000000L
val max = 400000000000000L

val part1 = hailstones.combinations(2)
  .flatMap { case List(a, b) => a.intersect(b) }
  .count { case (x, y) => x > min && x < max && y > min && y < max }