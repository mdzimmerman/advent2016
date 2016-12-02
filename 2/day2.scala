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

case class Button(x: Int, y: Int) {
  val size = 3

  def getValue(): Int = {
    (y-1) * size + x
  }

  def getNextButton(d: Dir): Button = {
    d match {
      case UP    => if (y == 1)    Button(x, y) else Button(x, y-1)
      case RIGHT => if (x == size) Button(x, y) else Button(x+1, y)
      case DOWN  => if (y == size) Button(x, y) else Button(x, y+1)
      case LEFT  => if (x == 1)    Button(x, y) else Button(x-1, y)
    }
  }
}

val b0 = Button(2, 2)
var b = b0
val instructions = List("ULL", "RRDDD", "LURDL", "UUUUD")

for (line <- instructions) {
  println(s"# $line #")
  for (d <- line.flatMap(getDirection)) {
    b = b.getNextButton(d)
    println(b)
  }
  println(s"Button: ${b.getValue()}")
}

//println(Button(1,1).getValue())
//println(Button(2,2).getValue())
//println(Button(3,3).getValue())