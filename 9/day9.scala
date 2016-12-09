import scala.annotation.tailrec

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
//println(process("ADVENT"))
//println(process("A(1x5)BC"))