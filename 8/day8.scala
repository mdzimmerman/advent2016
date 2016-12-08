import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by mzimmerman on 12/8/16.
  */

sealed trait Instruction

case class Rect(x: Int, y: Int)      extends Instruction
case class RotateCol(x: Int, n: Int) extends Instruction
case class RotateRow(y: Int, n: Int) extends Instruction

val RectString      = "rect (\\d+)x(\\d)".r
val RotateColString = "rotate column x=(\\d+) by (\\d+)".r
val RotateRowString = "rotate row y=(\\d+) by (\\d+)".r

def parseInstruction(s: String): Option[Instruction] = s match {
  case RectString(x, y)      => Some(Rect(x.toInt, y.toInt))
  case RotateColString(x, n) => Some(RotateCol(x.toInt, n.toInt))
  case RotateRowString(y, n) => Some(RotateRow(y.toInt, n.toInt))
  case _ => None
}

case class Screen(width: Int, height: Int) {
  val matrix = Array.ofDim[Boolean](height, width)
  for (j <- 0 until height; i <- 0 until width) matrix(j)(i) = false

  def printMatrix(): Unit = {
    for (j <- 0 until height) {
      for (i <- 0 until width) {
        if (matrix(j)(i)) print("#") else print(".")
      }
      println()
    }
  }

  def copy(): Screen = {
    val screenNew = Screen(width, height)
    for (j <- 0 until height; i <- 0 until width) screenNew.matrix(j)(i) = matrix(j)(i)
    screenNew
  }

  def execute(inst: Instruction): Screen = {
    val screenNew = copy()
    inst match {
      case r: Rect =>
        for (j <- 0 until r.y; i <- 0 until r.x)
          screenNew.matrix(j)(i) = true
      case r: RotateRow =>
        for (i <- 0 until width) {
          val iNew = (i + r.n) % width
          screenNew.matrix(r.y)(iNew) = matrix(r.y)(i)
        }
      case r: RotateCol =>
        for (j <- 0 until height) {
          val jNew = (j + r.n) % height
          screenNew.matrix(jNew)(r.x) = matrix(j)(r.x)
        }
    }
    screenNew
  }

  def count(): Int = {
    var total = 0
    for (j <- 0 until height; i <- 0 until width)
      if (matrix(j)(i)) total += 1
    total
  }
}

@tailrec
def processInstructions(screen: Screen, instructions: List[Instruction]): Screen = {
  screen.printMatrix()
  if (instructions.isEmpty) {
    screen
  }
  else {
    println(instructions.head)
    val screenNew = screen.execute(instructions.head)
    processInstructions(screenNew, instructions.tail)
  }
}

val test = List(
  "rect 3x2",
  "rotate column x=1 by 1",
  "rotate row y=0 by 4",
  "rotate column x=1 by 1"
).flatMap(parseInstruction)
val sTest = processInstructions(Screen(7, 3), test)
println(s"total = ${sTest.count}")

val input = Source.fromFile("input.txt").getLines.flatMap(parseInstruction).toList
val sInput = processInstructions(Screen(50, 6), input)
println(s"total = ${sInput.count}")