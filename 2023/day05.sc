import common.loadPackets
import scala.collection.immutable.NumericRange

val input = loadPackets(List("day05.txt"))

val seeds = input.head match {
  case s"seeds: ${seeds}" => seeds.split("\\s+").map(_.toLong)
}

type SeedRange = NumericRange[Long]
type SeedRanges = List[SeedRange]

case class Mapping(destinationRangeStart: Long, sourceRangeStart: Long, length: Long) {
  val offset = destinationRangeStart - sourceRangeStart
  val sourceRange = sourceRangeStart until sourceRangeStart + length
  val points: List[Long] = List(sourceRange.start, sourceRange.end)
  def pertains(source: Long): Boolean = sourceRange.contains(source)
  def apply(source: Long): Long = source + offset
  def apply(source: SeedRange): SeedRange = source.start + offset until source.end + offset
}

object Mappings {
  def parse(line: String): Mapping =
    line.split("\\s+").map(_.toLong).toList match {
      case List(a, b, c) => Mapping(a, b, c)
    }

  def parseMappings(lines: List[String]): List[List[Mapping]] =
    if (lines.isEmpty) List()
    else lines.dropWhile(_.isBlank).takeWhile(!_.isBlank).map(parse) ::
      parseMappings(lines.dropWhile(!_.isBlank).tail.tail)
}

val mappings = Mappings.parseMappings(input.tail.tail.tail)

def applyMappings(mappings: Seq[Mapping])(source: Long): Long =
  mappings.find(_.pertains(source)).map(_.apply(source)).getOrElse(source)

val part1 = mappings.map(applyMappings).foldLeft(seeds)((seeds, mapping) => seeds.map(mapping)).min

def splitRangeAtPoint(range: SeedRange, point: Long): SeedRanges =
  if (range.contains(point))
    List((range.start until point), (point until range.end)).filter(_.nonEmpty)
  else
    List(range)

def splitRangesAtPoints(ranges: SeedRanges, points: List[Long]): SeedRanges =
  points.foldLeft(ranges)((ranges, point) => ranges.flatMap(splitRangeAtPoint(_, point)))

val seedRanges: SeedRanges = seeds.grouped(2).map {
  case Array(from, size) => from until from + size
}.toList

def applyMappingsToRange(mappings: Seq[Mapping])(source: SeedRange): SeedRange =
  mappings.find(_.pertains(source.start)).map(_.apply(source)).getOrElse(source)

val part2 = mappings.foldLeft(seedRanges)((seeds, mappings) => 
    splitRangesAtPoints(seeds, mappings.flatMap(_.points)).map(applyMappingsToRange(mappings)))
  .map(_.start).min