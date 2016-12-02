import scala.collection.mutable
import scala.io.Source

val filename = "input.txt"

sealed trait Direction
case object NORTH extends Direction
case object EAST  extends Direction
case object SOUTH extends Direction
case object WEST  extends Direction

sealed trait Rotation
case object RIGHT extends Rotation
case object LEFT  extends Rotation

case class Point(x: Int, y: Int)

case class Position(x: Int, y: Int, direction: Direction) {
  def rotateAndTranslate(rotation: Rotation, distance: Int): Seq[Position] = {
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
    directionNew match {
      case NORTH =>
        (y+1 to y+distance).map(Position(x, _, NORTH)).toSeq
      case EAST  =>
        (x+1 to x+distance).map(Position(_, y, EAST)).toSeq
      case SOUTH =>
        (y-1 to y-distance by -1).map(Position(x, _, SOUTH)).toSeq
      case WEST  =>
        (x-1 to x-distance by -1).map(Position(_, y, WEST)).toSeq
    }
  }

  def getPoint(): Point = Point(this.x, this.y)

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
          p = p.rotateAndTranslate(LEFT, dist.toInt).last
          println(s"LEFT  $dist => $p")
        }
        case "R" => {
          p = p.rotateAndTranslate(RIGHT, dist.toInt).last
          println(s"RIGHT $dist => $p")
        }
        case _ =>
      }
    }
  }
  p.distance(pStart)
}

def calcDistanceFirstRevisit(directions: List[String], p0: Position): Int = {
  val directionRegex = """([RL])(\d+)""".r
  val pointHistory = mutable.ArrayBuffer[Point]()

  var p1 = p0
  for (d <- directions) {
    val pSeq = d match {
      case directionRegex(dir, dist) => dir match {
        case "L" => p1.rotateAndTranslate(LEFT, dist.toInt)
        case "R" => p1.rotateAndTranslate(RIGHT, dist.toInt)
        case _ => Seq(p1)
      }
      case _ => Seq(p1)
    }
    p1 = pSeq.last
    println(s"$d => $p1")
    for (p <- pSeq) {
      if (pointHistory.contains(p.getPoint)) {
        println(s"Match! => $p")
        return p.distance(p0)
      }
      pointHistory += p.getPoint
    }
  }
  p1.distance(p0)
}

val p0 = Position(0, 0, NORTH)

val lines = Source.fromFile(filename).getLines().toList
val directions = lines.flatMap(_.split(", "))

println(calcDistance(List("R2", "L3"), p0))
println(calcDistance(List("R2", "R2", "R2"), p0))
println(calcDistance(List("R5", "L5", "R5", "R3"), p0))
println(calcDistance(directions, p0))

println(calcDistanceFirstRevisit(List("R8", "R4", "R4", "R8"), p0))
println(calcDistanceFirstRevisit(directions, p0))


