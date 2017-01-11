import scala.io.Source

/**
  * Created by mzimmerman on 12/30/16.
  */

case class Range(val low: Long, val high: Long)

val range = """(\d+)-(\d+)""".r

val input = Source.fromFile("input.txt").getLines().flatMap{
  case range(low, high) => Some(Range(low.toLong, high.toLong))
  case _ => None
}.toSeq.sortBy(_.low)

var i = 0
//var port: Option[Long] = None
var ip: Long = 0
var ipFirst: Option[Long] = None
var count = 0
val max = scala.math.pow(2L, 32)-1
while (ip < max) {
  if ( ip >= input(i).low && ip <= input(i).high ) {
    ip = input(i).high + 1
    i += 1
  } else if ( ip > input(i).high ) {
    i += 1
  } else if ( ip < input(i).low ) {
    println(ip)
    count += 1
    if (ipFirst.isEmpty) ipFirst = Some(ip)
    //port = Some(ip)
    ip += 1
  }
}

println(s"first ip = $ipFirst")
println(s"count = $count")