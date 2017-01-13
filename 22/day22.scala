import scala.io.Source

/**
  * Created by matt on 1/10/2017.
  */
val NodeString = """/dev/grid/node-x(\d+)-y(\d+) +(\d+)T +(\d+)T +(\d+)T + (\d+)%""".r

case class Node(x: Int, y: Int, size: Int, used: Int, avail: Int, use: Int) {
  def name = s"node-x$x-y$y"
}

object AsInt {
  def unapply(s: String) =
    try {
      Some(s.toInt)
    } catch {
      case e: NumberFormatException => None
    }
}

class GridArray(input: List[String]) {
  val nodes  = parse(input)
  val height = nodes.length
  val width  = nodes(0).length

  def parse(in: List[String]) = {
    val nodeList = in.flatMap {
      case NodeString(AsInt(x), AsInt(y), AsInt(size), AsInt(used), AsInt(avail), AsInt(use)) =>
        Some(Node(x, y, size, used, avail, use))
      case _ =>
        None
    }
    val width  = nodeList.map(_.x).max+1
    val height = nodeList.map(_.y).max+1
    val nodes  = Array.ofDim[Node](height, width)
    for (n <- nodeList) {
      nodes(n.y)(n.x) = n
    }
    nodes
  }

  def printAll() = {
    for (j <- 0 until height) {
      for (i <- 0 until width) {
        val n = nodes(j)(i)
        if ( n.used > 100 ) {
          print(" # ")
        } else if ( n.used == 0 ) {
          print(" _ ")
        } else if ( i == 0 && j == 0 ) {
          print("(.)")
        } else if ( i == width-1 && j == 0 ) {
          print(" G ")
        } else {
          print(" . ")
        }
      }
      println()
    }
  }

  def countPairs() = {
    var count = 0
    for (a <- nodes.flatten; b <- nodes.flatten) {
      if (a.used > 0 && a != b && a.used <= b.avail) {
        println(s"${a.name} -> ${b.name}")
        count += 1
      }
    }
    count
  }
}

val i = new GridArray(Source.fromFile("input.txt").getLines().toList)
//println(i.countPairs())
println(i.width)
println(i.height)
i.printAll()

// 34 for moving  _ -> G
// 1 for shifting G left 1
// 33 * 5 for G -> (.)