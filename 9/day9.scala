import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by mzimmerman on 12/9/16.
  */

object AsInt {
  def unapply(s: String) =
    try {
      Some(s.toInt)
    } catch {
      case e: NumberFormatException => None
    }
}

val RepeatRegex = """(\((\d+)x(\d+)\)).+""".r

def process(in: String): String = process(in, "")

@tailrec
def process(in: String, out: String): String = {
  if (in.isEmpty) {
    out
  } else {
    //println(in)
    in match {
      case RepeatRegex(instr, AsInt(c), AsInt(n)) =>
        val inNew = in.substring(instr.length + c, in.length)
        val repeat = in.substring(instr.length, instr.length + c)
        val outNew = out + (0 until n).map(_ => repeat).mkString("")
        //println(s"instr=$instr count=$c n=$n repeat=$repeat outNew=$outNew")
        process(inNew, outNew)
      case _ =>
        //println(in.head)
        process(in.tail, out + in.head)
    }
  }
}

def count(in: String): Long = count(in, 0L)

def count(in: String, out: Long): Long = {
  if (in.isEmpty) {
    out
  } else {
    in match {
      case RepeatRegex(instr, AsInt(c), AsInt(n)) =>
        val inNew = in.substring(instr.length + c, in.length)
        val repeat = in.substring(instr.length, instr.length + c)
        val cRepeat = count(repeat, 0L)
        val outNew = out + (n * cRepeat)
        count(inNew, outNew)
      case _ =>
        count(in.tail, out+1)
    }
  }
}

val test = List(
  "ADVENT",
  "A(1x5)BC",
  "(3x3)XYZ",
  "A(2x2)BCD(2x2)EFG",
  "(6x1)(1x3)A",
  "X(8x2)(3x3)ABCY"
)
test.foreach(in => {
  val out = process(in)
  println(s"$in => $out [${out.length}]")
})

val input = Source.fromFile("input.txt").getLines().mkString("")
val out = process(input)
println(s"compressed   = ${input.length}")
println(s"uncompressed = ${out.length}")

val test2 = List(
  "(3x3)XYZ",
  "X(8x2)(3x3)ABCY",
  "(27x12)(20x12)(13x14)(7x10)(1x12)A",
  "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
)
test2.foreach(in => {
  val out = count(in)
  println(s"$in => $out")
})
val out2 = count(input)
println(s"compressed    = ${input.length}")
println(s"uncompressed2 = $out2")