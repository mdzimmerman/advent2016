import scala.io.Source

/**
  * Created by matt on 1/10/2017.
  */

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

val NodeString = """/dev/grid/node-x(\d+)-y(\d+) +(\d+)T +(\d+)T +(\d+)T + (\d+)%""".r

val input = Source.fromFile("input.txt").getLines().flatMap {
  case NodeString(AsInt(x), AsInt(y), AsInt(size), AsInt(used), AsInt(avail), AsInt(use)) =>
    Some(Node(x, y, size, used, avail, use))
  case _ =>
    None
}.toSeq

var count = 0
for (a <- input; b <- input) {
  if (a.used > 0 && a != b && a.used <= b.avail) {
    println(s"${a.name} -> ${b.name}")
    count += 1
  }
}
println(count)