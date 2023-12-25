import common.loadPackets

val input = loadPackets(List("day15.txt")).head.split(',')

def hashcode(step: String): Int = step.foldLeft(0)((current, ascii) => (current + ascii) * 17 % 256)

val part1 = input.map(hashcode).sum

case class Lens(label: String, focalLength: Int) {
  def focusingPower(boxNumber: Int, slotNumber: Int): Int = (1 + boxNumber) * slotNumber * focalLength
}

case class Box(lenses: List[Lens] = List()) {
  def replace(lens: Lens): Box =
    if (lenses.exists(_.label == lens.label))
      copy(lenses = lenses.map(current => if (current.label == lens.label) lens else current))
    else copy(lenses = lenses.appended(lens))

  def remove(label: String): Box = copy(lenses = lenses.filter(_.label != label))

  def focusingPower(boxNumber: Int): Int = lenses.zipWithIndex.map {
    case (lens, index) => lens.focusingPower(boxNumber, index + 1)
  }.sum
}

type LensConfiguration = Map[Int, Box]

def perform(state: LensConfiguration, operation: String): LensConfiguration = operation match {
  case s"${label}=${focalLength}" => state.updatedWith(hashcode(label))(_.map(_.replace(Lens(label, focalLength.toInt))))
  case s"${label}-" => state.updatedWith(hashcode(label))(_.map(_.remove(label)))
}

val part2 = input.foldLeft(Map().default(Box()): LensConfiguration)(perform).map {
  case (boxNumber, box) => box.focusingPower(boxNumber)
}.sum
