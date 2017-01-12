import scala.collection.mutable
import scala.io.Source

/**
  * Created by mzimmerman on 1/9/17.
  */

case class Coord(x: Int, y: Int)

case class Pair(i: Int, j: Int)

class Maze(lines: List[String], points: String) {
  val nrows = lines.length
  val ncols = lines.head.length
  val arr = lines.flatMap(_.toSeq).toSeq
  val pts = points.toSeq
  val ptsDists = calcPairwiseDists(pts)

  def get(c: Coord): Option[Char] = get(c.x, c.y)

  def get(x: Int, y: Int): Option[Char] = {
    val n = y * ncols + x
    if (n >= 0 && n < arr.length)
      Some(arr(n))
    else
      None
  }

  def getNextValid(c: Coord): List[Coord] = {
    List(
      Coord(c.x - 1, c.y),
      Coord(c.x, c.y - 1),
      Coord(c.x + 1, c.y),
      Coord(c.x, c.y + 1)
    ).filter(coord => get(coord) match {
      case Some(ncChar) if ncChar != '#' => true
      case _ => false
    })
  }

  def find(c: Char): Coord = {
    val n = arr.indexOf(c)
    val x = n % ncols
    val y = n / ncols
    //println(s"n=$n x=$x y=$y")
    Coord(x, y)
  }

  def printMaze() = {
    for (j <- 0 until nrows) {
      for (i <- 0 until ncols)
        print(get(i, j).get)
      println()
    }
  }

  def bfs(start: Coord, end: Coord): Option[Int] = {
    val visited = new mutable.HashSet[Coord]()
    val queue = new mutable.Queue[(Coord, Int)]()
    visited.add(start)
    queue.enqueue((start, 0))
    while (queue.nonEmpty) {
      val (c, length) = queue.dequeue()
      //println(s"## bfs: $c length=$length")
      for (cNext <- getNextValid(c)) {
        if (cNext == end) {
          return Some(length+1)
        } else if (!visited.contains(cNext)) {
          visited.add(cNext)
          queue.enqueue((cNext, length + 1))
        }
      }
    }
    None
  }

  def calcPairwiseDists(points: Seq[Char]) = {
    val dists = mutable.HashMap[Pair, Int]()
    val coords = points.map(find)
    for (i <- 0 until points.length ; j <- i until points.length) {
      val a = coords(i)
      val b = coords(j)
      if ( a == b ) {
        dists(Pair(i, j)) = 0
      } else {
        //println(s"## calc dist between $i-$j")
        val dist = bfs(a, b).getOrElse(0)
        dists(Pair(i, j)) = dist
        dists(Pair(j, i)) = dist
      }
    }
    dists.toMap
  }

  def printDists() = {
    print("%5s".format(" "))
    for (j <- 0 until pts.length)
      print(" %4d".format(j))
    println()
    for (i <- 0 until pts.length) {
      print("%5d".format(i))
      for (j <- 0 until pts.length) {
        print(" %4d".format(ptsDists(Pair(i, j))))
      }
      println()
    }
  }

  def findMinPath(): Int = {
    var dmin = 1000000
    val p0 = (0 until pts.length)
    val phead = Seq(p0.head)
    for (ptail <- p0.tail.permutations) {
      val p = phead ++ ptail ++ phead
      //println(p.mkString("-"))
      val d = p.sliding(2).map(s => {
        val p = Pair(s(0), s(1))
        ptsDists(p)
      }).sum
      println(s"$p => $d")
      if (d < dmin) dmin = d
    }
    dmin
  }
}

val test = new Maze(Source.fromFile("test.txt").getLines().toList, "01234")
//test.printMaze()
test.printDists()
println(test.findMinPath())

val input = new Maze(Source.fromFile("input.txt").getLines().toList, "01234567")
//input.printMaze()
input.printDists()
println(input.findMinPath())

