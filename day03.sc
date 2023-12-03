import common.loadPackets
import scala.util.matching.Regex

val input = loadPackets(List("day03.txt"))

val symbols = input.zipWithIndex
  .flatMap { case (line, rownum) =>
    line.zipWithIndex
      .flatMap {
        case ('.', _) => None
        case (c, colnum) if !c.isDigit => Some(rownum, colnum)
        case _ => None
      }
  }

val partNumberRegex = """\d+""".r

def getPartNumber(m: Regex.Match, linenum: Int): Option[Int] =
  (for (
    colnum <- m.start - 1 to m.start + m.group(0).length;
    rownum <- linenum - 1 to linenum + 1
    if symbols.contains((rownum, colnum))
  ) yield m.group(0).toInt).headOption

def findPartNumbers(line: String, linenum: Int): Iterator[Int] =
  partNumberRegex.findAllMatchIn(line)
    .flatMap(m => getPartNumber(m, linenum))

findPartNumbers(input.head, 0).toList

val part1 = input.zipWithIndex.flatMap {
  case (line, rownum) => findPartNumbers(line, rownum)
}.sum