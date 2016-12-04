import scala.io.Source

/**
  * Created by matt on 12/3/2016.
  */

//val lines = List("5  10  15")

def lineToSeq(s: String): Seq[Int] = {
  s.split(" +").filter(_ != "").map(_.toInt).toSeq
}

def validTriangle(in: Seq[Int]): Boolean = {
  if (in.length < 3)
    false
  else
    in(0) < (in(1) + in(2)) && in(1) < (in(0) + in(2)) && in(2) < (in(0) + in(1))
}

val test = List(
  "   5  10  15",
  "   5   5   5").map(lineToSeq)
val input = Source.fromFile("input.txt").getLines.toList.map(lineToSeq)

val valid = input.filter(validTriangle).length
println(s"total = ${input.length}")
println(s"valid = $valid")

