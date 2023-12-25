import common.loadPackets

val input = loadPackets(List("day04.txt"))

case class LotteryCard(cardNumber: Int, winning: List[Int], drawn: List[Int]) {
  val amountOfWinningNumbers = drawn.count(winning.contains)
  val value: Int = if (amountOfWinningNumbers > 0) Math.pow(2, amountOfWinningNumbers - 1).toInt else 0
  def dispenses(card: LotteryCard): Boolean =
    (cardNumber + 1 to cardNumber + amountOfWinningNumbers).contains(card.cardNumber)
}

object LotteryCards {
  val lineRegex = """Card\s+(\d+):(.*)\|(.*)""".r

  def parseNumbers(value: String): List[Int] = value.trim.split("\\s+").map(_.toInt).toList

  def parse(line: String): LotteryCard = line match {
    case lineRegex(cardnum, winningNumberString, numberString) =>
      LotteryCard(cardnum.toInt, parseNumbers(winningNumberString), parseNumbers(numberString))
  }
}

val lotteryCards = input.map(LotteryCards.parse)
val part1 = lotteryCards.map(_.value).sum

val part2 = lotteryCards.foldLeft(Map[Int, Int]())((amountOfCards, card) =>
    amountOfCards.updated(card.cardNumber,
      1 + lotteryCards.filter(_.dispenses(card)).map(_.cardNumber).map(amountOfCards).sum))
  .values.sum
