import Jama.Matrix

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

def abcd(h1: Hailstone, h2: Hailstone) = (
  Array(h2.velocity.y - h1.velocity.y,
    h1.velocity.x - h2.velocity.x,
    h1.pos.y - h2.pos.y,
    h2.pos.x - h1.pos.x).map(_.toDouble),
  (h1.velocity.x * h1.pos.y - h2.velocity.x * h2.pos.y + h2.pos.x * h2.velocity.y - h1.pos.x * h1.velocity.y).toDouble
)

var coefficients = hailstones.combinations(2).take(4).toArray.map {
  case List(h1, h2) => abcd(h1, h2)
}

val m: Matrix = new Matrix(coefficients.map(_._1))
val x: Matrix = new Matrix(coefficients.map(c => Array(c._2)))
val Array(a, b, d, e) = m.inverse().times(x).getArray.map(_(0)).map(_.round)

val h1 = hailstones.head
val t1 = (a - h1.pos.x) / (h1.velocity.x - d)

val h2 = hailstones(1)
val t2 = (a - h2.pos.x) / (h2.velocity.x - d)

val f = ((h1.pos.z - h2.pos.z) + t1 * h1.velocity.z - t2 * h2.velocity.z) / (t1 - t2)
val c = h1.pos.z + t1 * (h1.velocity.z - f)

val part2 = a + b + c