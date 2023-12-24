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

/*
 * Assume the stone we shoot starts at a, b, c @ d, e, f
 * If we look at the world from the point of view of the stone, then we are not moving.
 * All of the hailstones will be aiming to hit us right at the origin.
 * So transform each hailstone x, y, z @ dx, dy, dz to x-a, y-b, z-c @ dx-d, dy-e, dz-f
 * If it is going to hit the origin, then the vector from the origin to the starting position has to be
 * a multiple of its velocity. We only need two dimensions to solve this for a, b, d and e:
 * (x-a) : (y-b) = (dx-d) : (dy-e)
 * (x-a) * (dy-e) = (y-b) * (dx-d)
 * Fill in for two different hailstones x1, y1 @ dx1, dy1 and x2, y2 @ dx2, dy2 and subtract
 * to get a linear equation for a, b, d and e:
 * (dy2 - dy1) * a + (dx1 - dx2) * b + (y1 - y2) * d + (x2 - x1) * e = dx1 * y1 - dx2 * y2 + x2 * dy2 - x1 * dy1
 * Each combination j of hailstorms yields a different equation of the type
 * cj1 * a + cj2 * b + cj3 * c + cj4 * d = cj5
 * Take four such equations to form a matrix
 * ((c11, c12, c13, c14),   (a,  (c51,
 *  (c21, c22, c23, c24),    b,   c52,
 *  (c31, c32, c33, c34),    d,   c53,
 *  (c41, c42, c43, c44)) *  e) = c54)
 * Or, A * x = b
 * We can find x by inverting A and computing x = A^-1 * b
 */

var coefficients = hailstones.combinations(2).take(4).toArray.map { case List(h1, h2) =>
  (Array(h2.velocity.y - h1.velocity.y,
    h1.velocity.x - h2.velocity.x,
    h1.pos.y - h2.pos.y,
    h2.pos.x - h1.pos.x).map(_.toDouble),
    Array((h1.velocity.x * h1.pos.y - h2.velocity.x * h2.pos.y + h2.pos.x * h2.velocity.y - h1.pos.x * h1.velocity.y).toDouble))
}

val A: Matrix = new Matrix(coefficients.map(_._1))
val x: Matrix = new Matrix(coefficients.map(_._2))
val Array(a, b, d, e) = A.inverse().times(x).getArray.map(_(0)).map(_.round)

val h1 = hailstones.head
val t1 = (a - h1.pos.x) / (h1.velocity.x - d)

val h2 = hailstones(1)
val t2 = (a - h2.pos.x) / (h2.velocity.x - d)

val f = ((h1.pos.z - h2.pos.z) + t1 * h1.velocity.z - t2 * h2.velocity.z) / (t1 - t2)
val c = h1.pos.z + t1 * (h1.velocity.z - f)

val part2 = a + b + c