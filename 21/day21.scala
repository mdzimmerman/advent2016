import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by mzimmerman on 12/21/16.
  */

sealed trait Op {
  def process(s: String): String

  def unprocess(s: String): String

  def minMax(x: Int, y: Int) = if (x <= y) (x, y) else (y, x)

  def swap(s: String, x: Int, y: Int): String = {
    val (i0, i1) = minMax(x, y)
    val s1 = s.substring(0, i0)
    val s2 = s.substring(i0+1, i1)
    val s3 = s.substring(i1+1)
    s1 + s(i1) + s2 + s(i0) + s3
  }

  def rotate(s: String, x: Int): String = {
    if (x >= 0) {
      s.substring(x) + s.substring(0, x)
    } else {
      rotate(s, s.length + x)
    }
  }

  def move(s: String, x: Int, y: Int): String = {
    val inter = s.substring(0, x) + s.substring(x+1)
    inter.substring(0, y) + s.charAt(x) + inter.substring(y)
  }

  def reverse(s: String, x: Int, y: Int): String = {
    val (i0, i1) = minMax(x, y)
    val s1 = s.substring(0, i0)
    val s2 = s.substring(i1+1)
    s1 + s.substring(i0, i1+1).reverse + s2
  }
}

case class SwapPosition(x: Int, y: Int) extends Op {
  def process(s: String): String = swap(s, x, y)
  def unprocess(s: String): String = swap(s, x, y)
}

case class SwapLetter(x: Char, y: Char) extends Op {
  def process(s: String): String = swap(s, s.indexOf(x), s.indexOf(y))
  def unprocess(s: String): String = swap(s, s.indexOf(x), s.indexOf(y))
}

case class RotateLeft(x: Int) extends Op {
  def process(s: String): String = rotate(s, x)
  def unprocess(s: String): String = rotate(s, -x)
}

case class RotateRight(x: Int) extends Op {
  def process(s: String): String = rotate(s, -x)
  def unprocess(s: String): String = rotate(s, x)
}

case class RotateBased(x: Char) extends Op {
  def process(s: String): String = {
    val i = s.indexOf(x)
    val inter = rotate(s, -i-1)
    if (i >= 4) rotate(inter, -1) else inter
  }
  def unprocess(s: String): String = {
    // brute force
    var out = s.permutations.filter(rs => process(rs) == s).toList
    out.head
  }
}

case class Reverse(x: Int, y: Int) extends Op {
  def process(s: String): String = reverse(s, x, y)
  def unprocess(s: String): String = reverse(s, x, y)
}

case class Move(x: Int, y: Int) extends Op {
  def process(s: String): String = move(s, x, y)
  def unprocess(s: String): String = move(s, y, x)
}

val SwapPositionPattern = """swap position (\d+) with position (\d+)""".r
val SwapLetterPattern   = """swap letter (.) with letter (.)""".r
val RotateLeftPattern   = """rotate left (\d+) steps?""".r
val RotateRightPattern  = """rotate right (\d+) steps?""".r
val RotateBasedPattern  = """rotate based on position of letter (.)""".r
val ReversePattern      = """reverse positions (\d+) through (\d+)""".r
val MovePattern         = """move position (\d+) to position (\d+)""".r

object AsInt {
  def unapply(s: String): Option[Int] =
    try {
      Some(s.toInt)
    } catch {
      case e: NumberFormatException => None
    }
}

object AsChar {
  def unapply(s: String): Option[Char] = Some(s.toSeq.head)
}

def parse(s: String): Option[Op] = s match {
  case SwapPositionPattern(AsInt(x), AsInt(y)) => Some(SwapPosition(x, y))
  case SwapLetterPattern(AsChar(x), AsChar(y)) => Some(SwapLetter(x, y))
  case RotateLeftPattern(AsInt(x))             => Some(RotateLeft(x))
  case RotateRightPattern(AsInt(x))            => Some(RotateRight(x))
  case RotateBasedPattern(AsChar(x))           => Some(RotateBased(x))
  case ReversePattern(AsInt(x), AsInt(y))      => Some(Reverse(x, y))
  case MovePattern(AsInt(x), AsInt(y))         => Some(Move(x, y))
  case _ => None
}

def parse(list: List[String]): List[Op] = list.flatMap(parse(_))

@tailrec
def evaluate(ops: List[Op], s: String): String = {
  if (ops.isEmpty)
    s
  else
    evaluate(ops.tail, ops.head.process(s))
}

@tailrec
def unevaluate(ops: List[Op], s: String): String = {
  if (ops.isEmpty)
    s
  else {
    //println("# "+s)
    unevaluate(ops.tail, ops.head.unprocess(s))
  }
}

println("--- test ---")
val test = parse(List(
  "swap position 4 with position 0",
  "swap letter d with letter b",
  "reverse positions 0 through 4",
  "rotate left 1 step",
  "move position 1 to position 4",
  "move position 3 to position 0",
  "rotate based on position of letter b",
  "rotate based on position of letter d"
))
println(evaluate(test, "abcde"))
//println(evaluate(List(RotateRight(2)), "abcde"))
println()

println("--- test #2 ---")
println(unevaluate(test.reverse, "decab"))
println()

println("--- part #1 ---")
val input = parse(Source.fromFile("input.txt").getLines.toList)
println(evaluate(input, "abcdefgh"))
println()

println("--- part #2 ---")
println(unevaluate(input.reverse, "fbgdceah"))