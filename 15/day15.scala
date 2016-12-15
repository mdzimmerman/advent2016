import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by mzimmerman on 12/15/16.
  */

case class Disc(size: Int, start: Int) {
  def position(time: Int): Int = (time + start) % size
}

object Disc {
  val discString = """Disc #(\d) has (\d+) positions; at time=0, it is at position (\d+).""".r

  def parse(s: String): Option[Disc] = s match {
    case discString(n, size, start) => Some(Disc(size.toInt, start.toInt))
    case _ => None
  }

  def pass(t: Int, discs: List[Disc]): Boolean = pass(t, discs, true)

  @tailrec
  def pass(t: Int, discs: List[Disc], status: Boolean): Boolean = {
    if (!status)
      false // short-circuit
    else if (discs.isEmpty)
      status
    else {
      val d = discs.head
      //println(s"Checking disk ${d} at time t=${t + 1} => ${d.position(t + 1)}")
      pass(t + 1, discs.tail, d.position(t + 1) == 0)
    }
  }

  @tailrec
  def findPassTime(discs: List[Disc], t: Int): Int = {
    //if (t % 10000 == 0) println(s"t=$t")
    if (pass(t, discs))
      t
    else
      findPassTime(discs, t + 1)
  }
}

val test = List(
  Disc(5, 4),
  Disc(2, 1)
)
println(test)
println(Disc.findPassTime(test, 0))

val input = Source.fromFile("input.txt").getLines.toList.flatMap(Disc.parse)
println(input)
println(Disc.findPassTime(input, 0))

val input2 = input ::: List(Disc(11, 0))
println(input2)
println(Disc.findPassTime(input2, 0))
