import scala.io.Source
import scala.util.matching.Regex

val filename = "input.txt"

sealed trait Direction
case object NORTH extends Direction
case object EAST  extends Direction
case object SOUTH extends Direction
case object WEST  extends Direction

sealed trait Rotation
case object RIGHT extends Rotation
case object LEFT  extends Rotation

case class Position(x: Int, y: Int, direction: Direction) {
  def rotateAndTranslate(rotation: Rotation, distance: Int): Position = {
    val directionNew: Direction = rotation match {
      case RIGHT => direction match {
        case NORTH => EAST
        case EAST  => SOUTH
        case SOUTH => WEST
        case WEST  => NORTH
      }
      case LEFT => direction match {
        case NORTH => WEST
        case WEST  => SOUTH
        case SOUTH => EAST
        case EAST  => NORTH
      }
    }
    val xNew = directionNew match {
      case EAST => x + distance
      case WEST => x - distance
      case _ => x
    }
    val yNew = directionNew match {
      case NORTH => y + distance
      case SOUTH => y - distance
      case _ => y
    }
    new Position(xNew, yNew, directionNew)
  }

  def distance(p: Position): Int = {
    Math.abs(this.x-p.x) + Math.abs(this.y-p.y)
  }
}

def calcDistance(directions: List[String], pStart: Position): Int = {
  val directionRegex = """([RL])(\d+)""".r
  var p = pStart
  println(p)
  for (d <- directions) {
    d match {
      case directionRegex(dir, dist) => dir match {
        case "L" => {
          p = p.rotateAndTranslate(LEFT, dist.toInt)
          println(s"LEFT  $dist => $p")
        }
        case "R" => {
          p = p.rotateAndTranslate(RIGHT, dist.toInt)
          println(s"RIGHT $dist => $p")
        }
        case _ =>
      }
    }
  }
  p.distance(pStart)
}

val p0 = Position(0, 0, NORTH)

println(calcDistance(List("R2", "L3"), p0))
println(calcDistance(List("R2", "R2", "R2"), p0))
println(calcDistance(List("R5", "L5", "R5", "R3"), p0))

val lines = Source.fromFile(filename).getLines().toList
val directions = lines.flatMap(_.split(", "))
println(calcDistance(directions, p0))
