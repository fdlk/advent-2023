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
  h2.velocity.y - h1.velocity.y,
  h1.velocity.x - h2.velocity.x,
  h1.pos.y - h2.pos.y,
  h2.pos.x - h1.pos.x,
  h1.velocity.x * h1.pos.y - h2.velocity.x * h2.pos.y + h2.pos.x * h2.velocity.y - h1.pos.x * h1.velocity.y
)

var coefficients = hailstones.combinations(2).map {
  case List(h1, h2) => abcd(h1, h2)
}.toList

coefficients.mkString("\n")
/*
Wolfram alpha:
invert matrix {(117,69,102814695089976,16915576153641), (66,78,137613439140084,5835907432620), (-552,199,-264204312577182,87881303532876), (-251,-29,-137736616452020,51123614927205)} * (47990945912805363, 28596076416021090, -92338590912830013, -90196889386570431)

1/37309861881398479661210980084296(344136196137413521223503801716 | -333009893657435680943585345852 | 119170059201711136859759904 | -76057218276458952860096635764
135350610042904326114990998946 | -105630423177850106104659597372 | 141694415024744561209868527644 | -276298420909869715678490094858
-284203253948040771 | 504895499177394386 | -75210509619738294 | 165687344796345807
1000675684047809906 | -334604035453744024 | -121669631635684860 | 646043128910536638)
*/

val det = BigInt("37309861881398479661210980084296")

val a1 = List(BigInt("344136196137413521223503801716"),
  BigInt("-333009893657435680943585345852"),
  BigInt("119170059201711136859759904"),
  BigInt("-76057218276458952860096635764"))
val a2 = List(BigInt("135350610042904326114990998946"),
  BigInt("-105630423177850106104659597372"),
  BigInt("141694415024744561209868527644"),
  BigInt("-276298420909869715678490094858"))

val x = List(47990945912805363L, 28596076416021090L, -92338590912830013L, -90196889386570431L)

def inner(a: List[BigInt], x: List[Long]) = a.zip(x).map{case (a, b) => a * b}.sum
val a = inner(a1, x) / det
val b = inner(a2, x) / det
val d = -193L
val e = -230L

val h1 = hailstones.head
val t1 = (a - h1.pos.x) / (h1.velocity.x - d)

val h2 = hailstones(1)
val t2 = (a - h2.pos.x) / (h2.velocity.x - d)

val f = ((h1.pos.z - h2.pos.z) + t1 * h1.velocity.z - t2 * h2.velocity.z) / (t1 - t2)
val c = h1.pos.z + t1 * (h1.velocity.z - f)

val part2 = a + b + c