import scala.io.Source

/**
  * Created by mzimmerman on 12/2/16.
  */

sealed trait Dir
case object UP extends Dir
case object RIGHT extends Dir
case object DOWN  extends Dir
case object LEFT  extends Dir

def getDirection(c: Char): Option[Dir] = {
  c match {
    case 'U' => Some(UP)
    case 'R' => Some(RIGHT)
    case 'D' => Some(DOWN)
    case 'L' => Some(LEFT)
    case _ => None
  }
}

trait Button {
  def getName: String

  def getNextButton(d: Dir): Button
}

case class OldButton(x: Int, y: Int) extends Button {
  val size = 3

  def getName: String = {
    ((y-1) * size + x).toString
  }

  def getNextButton(d: Dir): Button = {
    d match {
      case UP    => if (y == 1)    OldButton(x, y) else OldButton(x, y-1)
      case RIGHT => if (x == size) OldButton(x, y) else OldButton(x+1, y)
      case DOWN  => if (y == size) OldButton(x, y) else OldButton(x, y+1)
      case LEFT  => if (x == 1)    OldButton(x, y) else OldButton(x-1, y)
    }
  }
}

case class Position(x: Int, y: Int)

case class NewButton(position: Position) extends Button {
  val validButtons = Map[Position, String](
    Position(3, 1) -> "1",
    Position(2, 2) -> "2",
    Position(3, 2) -> "3",
    Position(4, 2) -> "4",
    Position(1, 3) -> "5",
    Position(2, 3) -> "6",
    Position(3, 3) -> "7",
    Position(4, 3) -> "8",
    Position(5, 3) -> "9",
    Position(2, 4) -> "A",
    Position(3, 4) -> "B",
    Position(4, 4) -> "C",
    Position(3, 5) -> "D"
  )

  def getName: String = validButtons.get(this.position) match {
    case Some(label) => label
    case None => "*"
  }

  def getNextButton(d: Dir): Button = {
    val nextPosition = d match {
      case UP    => Position(position.x, position.y-1)
      case RIGHT => Position(position.x+1, position.y)
      case DOWN  => Position(position.x, position.y+1)
      case LEFT  => Position(position.x-1, position.y)
    }
    validButtons.get(nextPosition) match {
      case Some(p) => NewButton(nextPosition)
      case None    => NewButton(position)
    }
  }
}

def parseInstructions(instructions: List[String], b0: Button) = {
  var b: Button = b0

  def parseDirs(inputButton: Button, dirs: List[Dir]): Button =
    if (dirs.isEmpty)
      inputButton
    else
      parseDirs(inputButton.getNextButton(dirs.head), dirs.tail)

  for (line <- instructions) {
    //println(s"# $line #")
    val dirs = line.flatMap(getDirection).toList
    b = parseDirs(b, dirs)
    print(b.getName)
  }
  println()
}

val instruct1 = List("ULL", "RRDDD", "LURDL", "UUUUD")
val instruct2 = Source.fromFile("input.txt").getLines().toList

print("Test  = ")
parseInstructions(instruct1, OldButton(2, 2))
print("Input = ")
parseInstructions(instruct2, OldButton(2, 2))

println()

print("Test  = ")
parseInstructions(instruct1, NewButton(Position(1, 3)))
print("Input = ")
parseInstructions(instruct2, NewButton(Position(1, 3)))
