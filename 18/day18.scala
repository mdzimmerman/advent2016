import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by mzimmerman on 12/28/16.
  */

def getTrap(prevLine: String, n: Int): Char = {
  val prevSeq = prevLine.toSeq
  val left   = isTrap(prevSeq, n-1)
  val center = isTrap(prevSeq, n)
  val right  = isTrap(prevSeq, n+1)
  if (left && center && !right)
    '^'
  else if (!left && center && right)
    '^'
  else if (left && !center && !right)
    '^'
  else if (!left && !center && right)
    '^'
  else
    '.'
}

def isTrap(line: Seq[Char], n: Int): Boolean = {
  if (n < 0 || n >= line.length)
    false
  else if (line(n) == '^')
    true
  else
    false
}

def getLine(prevLine: String): String = {
  (0 until prevLine.length).map(getTrap(prevLine, _)).mkString("")
}

@tailrec
def getLines(lines: List[String], nLines: Int): List[String] = {
  if (lines.length == nLines)
    lines.reverse
  else {
    val next = getLine(lines.head)
    getLines(next :: lines, nLines)
  }
}

def countAllSafe(line: String, totalLines: Int): Int = countAllSafe(line, totalLines, 0, 0)

@tailrec
def countAllSafe(line: String, totalLines: Int, n: Int, sum: Int): Int = {
  if (n == totalLines)
    sum
  else {
    val count = line.toList.count(_ == '.')
    val next = getLine(line)
    countAllSafe(next, totalLines, n + 1, sum + count)
  }
}

def countSafe(lines: List[String]): Int =
  lines.map(l => l.toList.count(_ == '.')).sum


println("--- test 1 ---")
val test = List("..^^.")
val testOut = getLines(test, 3)
for (t <- testOut) println(t)
println()

println("--- test 2 ---")
val test2 = List(".^^.^.^^^^")
val test2Out = getLines(test2, 10)
for (t <- test2Out) println(t)
println(countSafe(test2Out))
println()

println("--- part 1 ---")
val input = Source.fromFile("input.txt").getLines.toList
val inputOut = getLines(input, 40)
for (i <- inputOut) println(i)
println(countSafe(inputOut))
println(countAllSafe(input.head, 40))
println()

println("--- part 2 ---")
println(countAllSafe(input.head, 400000))