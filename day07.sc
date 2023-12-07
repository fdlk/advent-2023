import common.loadPackets
import scala.math.Ordering.Implicits.seqOrdering

val input = loadPackets(List("day07.txt"))

object Cards extends Enumeration {
  type Card = Value
  val Joker = Value("*")
  val Two = Value("2")
  val Three = Value("3")
  val Four = Value("4")
  val Five = Value("5")
  val Six = Value("6")
  val Seven = Value("7")
  val Eight = Value("8")
  val Nine = Value("9")
  val Ten = Value("T")
  val Jack = Value("J")
  val Queen = Value("Q")
  val King = Value("K")
  val Ace = Value("A")
}

case class Hand(cards: List[Cards.Card], bid: Int) {
  val counts: List[Int] = count(cards)
  val cardsWithJoker: List[Cards.Card] = cards.map {
    case Cards.Jack => Cards.Joker
    case other => other
  }
  val countsWithJoker: List[Int] = count(cardsWithJoker.filter(_ != Cards.Joker)) match {
    case most :: rest => most + cardsWithJoker.count(_ == Cards.Joker) :: rest
    case Nil => List(5)
  }

  def count(cards: List[Cards.Card]): List[Int] =
    cards.groupMapReduce(card => card)(_ => 1)(_ + _).values.toList.sorted.reverse
}

val hands: List[Hand] = input.map {
  case s"$cards $bid" => Hand(cards.map(_.toString).map(Cards.withName).toList, bid.toInt)
}

def ranks(hands: List[Hand]): Int = hands.map(_.bid).zipWithIndex.map { case (bid, index) => bid * (index + 1) }.sum

val part1 = ranks(hands.sortBy(hand => (hand.counts, hand.cards)))
val part2 = ranks(hands.sortBy(hand => (hand.countsWithJoker, hand.cardsWithJoker)))
