import scala.io.Source

/**
  * Created by mzimmerman on 1/9/17.
  */

case class Coord(x: Int, y: Int)

class Maze(lines: List[String], points: String) {
  val nrows = lines.length
  val ncols = lines.head.length
  val arr = lines.flatMap(_.toSeq).toSeq
  var pts = points.toSeq

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
    val queue = new scala.collection.mutable.Queue[(Coord, Int)]()
    queue.enqueue((start, 0))
    while (queue.nonEmpty) {
      val (c, length) = queue.dequeue()
      for (cNext <- getNextValid(c)) {
        if (cNext == end) {
          return Some(length+1)
        } else {
          queue.enqueue((cNext, length + 1))
        }
      }
    }
    None
  }
}

val test = new Maze(Source.fromFile("test.txt").getLines().toList, "01234")
test.printMaze()
val pt0 = test.find('0')
val pt1 = test.find('1')
val pt2 = test.find('2')
val pt3 = test.find('3')
val pt4 = test.find('4')

println("0 - 1 => "+test.bfs(pt0, pt1))
println("1 - 2 => "+test.bfs(pt1, pt2))
println("2 - 3 => "+test.bfs(pt2, pt3))
println("3 - 4 => "+test.bfs(pt3, pt4))
