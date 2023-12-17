import java.io.File
import scala.annotation.tailrec
import fastparse._
import NoWhitespace._

import scala.collection.convert.ImplicitConversions.`iterable AsScalaIterable`

package object common {

  /** An alias for the `Nothing` type.
   * Denotes that the type should be filled in.
   */
  type ??? = Nothing

  /** An alias for the `Any` type.
   * Denotes that the type should be filled in.
   */
  type *** = Any


  /**
   * Get a child of a file. For example,
   *
   * subFile(homeDir, "b", "c")
   *
   * corresponds to ~/b/c
   */
  def subFile(file: File, children: String*): File = {
    children.foldLeft(file)((file, child) => new File(file, child))
  }

  /**
   * Get a resource from the `src/main/resources` directory. Eclipse does not copy
   * resources to the output directory, then the class loader cannot find them.
   */
  def resourceAsStreamFromSrc(resourcePath: List[String]): Option[java.io.InputStream] = {
    val classesDir = new File(getClass.getResource(".").toURI)
    val projectDir = classesDir.getParentFile.getParentFile.getParentFile.getParentFile
    val resourceFile = subFile(projectDir, "src" :: "main" :: "resources" :: resourcePath: _*)
    if (resourceFile.exists)
      Some(new java.io.FileInputStream(resourceFile))
    else
      None
  }

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) / 1000.0 + "s")
    result
  }

  def loadPackets(dictionaryPath: List[String]): List[String] = {
    val wordstream = Option {
      getClass.getClassLoader.getResourceAsStream(dictionaryPath.mkString("/"))
    } orElse {
      resourceAsStreamFromSrc(dictionaryPath)
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }

  // https://stackoverflow.com/questions/40875537/fp-lcm-in-scala-in-1-line
  def lcm(list: Seq[Long]): Long = list.foldLeft(1: Long) {
    (a, b) => b * a / LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
  }

  trait Grid[T] {
    def heuristicDistanceToFinish(from: T): Int

    def getNeighbours(state: T): Iterable[T]

    def moveCost(from: T, to: T): Int
  }


  def aStarSearch[T](start: T, grid: Grid[T], isFinished: T => Boolean): Option[Int] = {
    case class NodeInfo(node: T, costFromStart: Int, estimatedTotalCost: Int) extends Comparable[NodeInfo] {

      override def compareTo(o: NodeInfo): Int = estimatedTotalCost.compareTo(o.estimatedTotalCost)
    }
    val open: java.util.PriorityQueue[NodeInfo] = new java.util.PriorityQueue()
    open.add(NodeInfo(start, 0, grid.heuristicDistanceToFinish(start)))

    @tailrec
    def loop(closed: Set[T]): Option[Int] = {
      if (open.isEmpty) return None
      val NodeInfo(current, currentCostFromStart, estimatedTotalCost) = open.poll()
      if (isFinished(current)) return Some(estimatedTotalCost)
      grid.getNeighbours(current)
        .filterNot(closed.contains)
        .foreach {
          neighbor => {
            val neighborCostFromStart = currentCostFromStart + grid.moveCost(current, neighbor)
            if (!open.exists(nodeInfo => nodeInfo.node == neighbor && nodeInfo.costFromStart <= neighborCostFromStart)) {
              open.removeIf(_.node == neighbor)
              open.offer(NodeInfo(neighbor, neighborCostFromStart, neighborCostFromStart + grid.heuristicDistanceToFinish(neighbor)))
            }
          }
        }
      loop(closed + current)
    }
    loop(Set())
  }



  private def space[_: P] = P(CharsWhileIn(" \r\n", 0))

  private def digits[_: P] = P(CharsWhileIn("0-9"))

  private def integral[_: P] = P("0" | CharIn("1-9") ~ digits.?)

  private def number[_: P] = P(CharIn("+\\-").? ~ integral).!.map(
    x => x.toInt
  )

  private def array[_: P] =
    P("[" ~/ expr.rep(sep = ","./) ~ space ~ "]").map(List(_: _*))

  def expr[_: P]: P[Any] = P(space ~ (array | number) ~ space)


}
