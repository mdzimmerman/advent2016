import scala.annotation.tailrec

/**
  * Created by mzimmerman on 12/16/16.
  */

def dragon(s: String): String = {
  val a = s
  val b = a.toSeq.reverse.map(_ match {
    case '1' => '0'
    case '0' => '1'
    case c => c
  }).mkString("")
  a + "0" + b
}

def checksum(s: String): String = checksum(s, 1)

@tailrec
def checksum(s: String, step: Int): String = {
  println(s"checksum $step: size = ${s.length}")
  if (s.length % 2 == 1)
    s
  else {
    val sNew = s.toSeq.sliding(2, 2).map(x => if (x(0) == x(1)) '1' else '0').mkString("")
    checksum(sNew, step+1)
  }
}

def fillDisk(s: String, size: Int): String = fillDisk(s, size, 1)

@tailrec
def fillDisk(s: String, size: Int, step: Int): String = {
  println(s"fillDisk $step: size = ${s.length}")
  if (s.length >= size)
    s.take(size)
  else
    fillDisk(dragon(s), size, step+1)
}

val test = List(
  "1",
  "0",
  "11111",
  "111100001010"
)
println("--- test dragon() ---")
for (s <- test) {
  println(s"$s => ${dragon(s)}")
}
println()

println("--- test checksum() ---")
val test2 = List(
  "110010110100"
)
for (s <- test2)
  println(s"$s => ${checksum(s)}")
println()

println("--- test fillDisk() ---")
val f = fillDisk("10000", 20)
println(s"fillDisk(10000, 20) = $f")
println(s"checksum($f) = ${checksum(f)}")
println()

println("--- Part 1 ---")
val input = "00111101111101000"
val size = 272
println(s"input = $input  size = $size")
println(s"checksum = ${checksum(fillDisk(input, size))}")
println()

println("--- Part 2 ---")
val size2 = 35651584
println(s"input = $input  size = $size2")
println(s"checksum = ${checksum(fillDisk(input, size2))}")
println()

