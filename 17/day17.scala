import java.math.BigInteger
import java.security.MessageDigest

import scala.collection.mutable

/**
  * Created by matt on 1/7/2017.
  */

case class Room(x: Int, y: Int) {
  val size = 4

  def getNext(dir: String): Option[Room] = {
    val (nx, ny) = dir match {
      case "U" => (x, y-1)
      case "D" => (x, y+1)
      case "L" => (x-1, y)
      case "R" => (x+1, y)
      case _ => (-1, -1)
    }
    if (nx >= 0 && nx < size && ny >= 0 && ny < size)
      Some(Room(nx, ny))
    else
      None
  }
}

case class Maze(prefix: String, start: Room = Room(0, 0), end: Room = Room(3, 3)) {
  val dirs = Map(
    0 -> "U",
    1 -> "D",
    2 -> "L",
    3 -> "R")
  private val m = MessageDigest.getInstance("MD5")

  def md5(s: String): String = {
    m.reset()
    m.update(s.getBytes())
    val digest = m.digest()
    val bigInt = new BigInteger(1, digest)
    "%032x".format(bigInt)
  }

  def isDoorOpen(check: String, i: Int) = {
    Integer.parseInt(check(i).toString, 16) > 10
  }

  def doors(room: Room, path: String): List[(Room, String)] = {
    val s = prefix + path
    val check = md5(s)
    dirs.flatMap { case (i, dir) =>
      val nextRoom = room.getNext(dir)
      if (nextRoom.isDefined && isDoorOpen(check, i))
        Some((nextRoom.get, path + dir))
      else
        None
    }.toList
  }

  def bfs(): Option[String] = {
    val queue = new mutable.Queue[(Room, String)]()
    queue.enqueue((start, ""))
    while (queue.nonEmpty) {
      val (room, path) = queue.dequeue()
      for ((roomNext, pathNext) <- doors(room, path)) {
        //println(s"# $roomNext $pathNext")
        if (roomNext == end) {
          return Some(pathNext)
        } else {
          queue.enqueue((roomNext, pathNext))
        }
      }
    }
    None
  }

  def longest(): Int = {
    var i = -1
    val queue = new mutable.Queue[(Room, String)]()
    queue.enqueue((start, ""))
    while (queue.nonEmpty) {
      val (room, path) = queue.dequeue()
      for ((roomNext, pathNext) <- doors(room, path)) {
        //println(s"# $roomNext $pathNext")
        if (roomNext == end) {
          i = pathNext.length
        } else {
          queue.enqueue((roomNext, pathNext))
        }
      }
    }
    i
  }
}

val m = Maze("hijkl")
println(m)
println(m.bfs())

val m2 = Maze("ihgpwlah")
println(m2)
println(m2.bfs())
println(m2.longest)

val m3 = Maze("kglvqrro")
println(m3)
println(m3.bfs())
println(m3.longest)

val m4 = Maze("ulqzkmiv")
println(m4)
println(m4.bfs())
println(m4.longest)

val i = Maze("pvhmgsws")
println(i)
println(i.bfs())
println(i.longest)