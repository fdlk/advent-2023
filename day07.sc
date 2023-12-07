import common.loadPackets
import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.seqOrdering

val input = loadPackets(List("day07.txt"))

object Cards extends Enumeration {
  type Card = Value

  val Ace = Value(1, "A")
  val King = Value(2, "K")
  val Queen = Value(3, "Q")
  val Jack = Value(4, "J")
  val Ten = Value(5, "T")
  val Nine = Value(6, "9")
  val Eight = Value(7, "8")
  val Seven = Value(8, "7")
  val Six = Value(9, "6")
  val Five = Value(10, "5")
  val Four = Value(11, "4")
  val Three = Value(12, "3")
  val Two = Value(13, "2")
}

object HandTypes extends Enumeration {
  type HandType = Value

  val FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair, HighCard = Value
}

case class Hand(cards: List[Cards.Card], bid: Int) extends Ordered[Hand]{

  val handType: HandTypes.HandType = {
    cards.groupMapReduce(card => card)(_ => 1)(_+_).values.toList.sorted match {
      case List(5) => HandTypes.FiveOfAKind
      case List(1, 4) => HandTypes.FourOfAKind
      case List(2, 3) => HandTypes.FullHouse
      case List(1, 1, 3) => HandTypes.ThreeOfAKind
      case List(1, 2, 2) => HandTypes.TwoPair
      case x if x.contains(2) => HandTypes.OnePair
      case _ => HandTypes.HighCard
    }
  }
  def compare(that: Hand): Int = (handType, cards) compare (that.handType, that.cards)
}

def parseHand(hand: String): Hand = hand match {
  case s"$cards $bid" => Hand(
    cards.map(card => Cards.values.find(v => v.toString.charAt(0) == card).get).toList,
    bid.toInt)
}

val hands: List[Hand] = input.map(parseHand)

hands.sorted.reverse.map(_.bid).zipWithIndex.map{case (bid, index) => bid * (index + 1)}.sum