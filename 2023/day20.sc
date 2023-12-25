import common.{lcm, loadPackets}
import scala.annotation.tailrec

sealed trait HighOrLow
case object High extends HighOrLow
case object Low extends HighOrLow

sealed trait OnOrOff
case object On extends OnOrOff
case object Off extends OnOrOff

val input = loadPackets(List("day20.txt"))

type Destinations = Map[String, List[String]]
val regex = """([%&]?)(\w+) -> (.+)""".r
val destinations: Destinations = input.map({
  case regex(_, from, to) => from -> to.split(", ").toList
}).toMap

type FlipFlops = Map[String, OnOrOff]
val flipFlops: FlipFlops = input.flatMap {
  case regex("%", name, _) => Some(name -> Off)
  case _ => None
}.toMap

type Conjunctions = Map[String, Map[String, HighOrLow]]
val conjunctions: Conjunctions = input.flatMap {
  case regex("&", name, _) => Some(name -> Map[String, HighOrLow]().withDefaultValue(Low))
  case _ => None
}.toMap[String, Map[String, HighOrLow]]

val inputs: Map[String, List[String]] = conjunctions.keys.map(conjunction =>
    conjunction -> destinations.filter(_._2.contains(conjunction)).keys.toList).toMap

case class Pulse(from: String, pulse: HighOrLow, to: String)

case class State(flipFlops: FlipFlops, conjunctions: Conjunctions, pulses: List[Pulse]) {
  def dropPulse(): State = copy(pulses = pulses.tail)

  def flipFlop(pulse: HighOrLow, to: String): State = (pulse, flipFlops(to)) match {
    case (Low, Off) => copy(flipFlops = flipFlops.updated(to, On)).emitFlipFlop().dropPulse()
    case (Low, On) => copy(flipFlops = flipFlops.updated(to, Off)).emitFlipFlop().dropPulse()
    case (High, _) => dropPulse()
  }

  def emitFlipFlop(): State = {
    val from = pulses.head.to
    val emittedPulse = flipFlops(from) match {
      case On => High
      case Off => Low
    }
    copy(pulses = pulses ::: destinations(from).map(to => Pulse(from, emittedPulse, to)))
  }

  def conjunction(pulse: HighOrLow, from: String, to: String): State = {
    val lastPulse: Map[String, HighOrLow] = conjunctions(to).updated(from, pulse)
    val pulseEmitted = if (inputs(to).map(lastPulse).forall(_ == High)) Low else High
    val emitted = destinations(to).map(destination => Pulse(to, pulseEmitted, destination))
    copy(conjunctions = conjunctions.updated(to, lastPulse), pulses = pulses.tail ::: emitted)
  }

  def broadcast(pulse: HighOrLow): State =
    copy(pulses = pulses.tail :::
      destinations("broadcaster").map(destination => Pulse("broadcaster", pulse, destination)))

  def button(): State = pulses match {
    case Nil => copy(pulses = List(Pulse("button", Low, "broadcaster")))
  }

  def next(): State = pulses.head match {
    case Pulse(_, pulse, to) if flipFlops.keySet(to) => flipFlop(pulse, to)
    case Pulse(from, pulse, to) if conjunctions.keySet(to) => conjunction(pulse, from, to)
    case Pulse(_, pulse, "broadcaster") => broadcast(pulse)
    case _ => dropPulse()
  }
}
val initialState = State(flipFlops = flipFlops, conjunctions = conjunctions, pulses = List())

def pushButtonPulses(state: State): List[Pulse] =
  LazyList.iterate(state.button())(_.next()).takeWhile(_.pulses.nonEmpty)
    .map(_.pulses.head).toList

def pushButtonAndWait(state: State): (State, Int, Int) = {
  val states = LazyList.iterate(state.button())(_.next()).takeWhile(_.pulses.nonEmpty)
  val counts = states.groupMapReduce(_.pulses.head.pulse)(_ => 1)(_ + _)
  (states.last.next(), counts(High), counts(Low))
}

val iterations = LazyList.iterate((initialState, 0, 0)) { case (state, highs, lows) =>
  val (s, h, l) = pushButtonAndWait(state)
  (s, highs + h, lows + l)
}

val (_, highs, lows) = iterations(1000)
val part1 = highs * lows * 1L

@tailrec
def buttonPressesTillInputFires(input: String)(state: State, buttonPresses: Long = 0): Long = {
  val iterations = LazyList.iterate(state.button())(_.next()).takeWhile(_.pulses.nonEmpty).toList
  if (iterations.map(_.pulses.head).exists {
    case Pulse(from, High, "ls") => from == input
    case _ => false
  })
    buttonPresses
  else buttonPressesTillInputFires(input)(iterations.last.next(), buttonPresses + 1)
}

val part2 = lcm(inputs("ls").map(buttonPressesTillInputFires(_)(initialState) + 1))