import scala.collection.mutable
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

val input2 = Source.fromFile("input.txt").getLines

val out = new mutable.MutableList[Seq[Int]]()

while (input2.hasNext) {
  val s1 = lineToSeq(input2.next)
  val s2 = lineToSeq(input2.next)
  val s3 = lineToSeq(input2.next)
  out += Seq(s1(0), s2(0), s3(0))
  out += Seq(s1(1), s2(1), s3(1))
  out += Seq(s1(2), s2(2), s3(2))
}

val valid2 = out.filter(validTriangle).length
println(s"valid = $valid2")