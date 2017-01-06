// requires ``scala -J-Xmx1g`` (or more)

import scala.annotation.tailrec

case class Elf(id: Integer) {
  var prev: Option[Elf] = None
  var next: Option[Elf] = None

  def getPrev: Elf = prev.get

  def getNext: Elf = next.get

  def delete(): Unit = {
    prev.foreach(p => p.next = next)
    next.foreach(n => n.prev = prev)
  }

  // generate a list of the circle, starting at the current Elf
  // (by starting at the end and working forward)
  def getCircle(): List[Elf] = getCircle(List(getPrev))

  @tailrec
  private def getCircle(circle: List[Elf]): List[Elf] = {
    if (circle.head == this)
      circle
    else
      getCircle(circle.head.getPrev :: circle)
  }
}

object Elf {
  def buildCircle(n: Int): Vector[Elf] = {
    val elves = (1 to n).map(Elf(_)).toVector
    for (i <- 0 until n - 1) {
      elves(i).next = Some(elves(i + 1))
      elves(i + 1).prev = Some(elves(i))
    }
    elves.head.prev = Some(elves.last)
    elves.last.next = Some(elves.head)
    elves
  }
}

def solve(n: Int): Elf = {
  val elves = Elf.buildCircle(n)

  var start = elves.head
  var next  = start.getNext
  for (i <- 0 until n-1) {
    if (i % 1000 == 0) println(s"step $i: $start <- $next")
    next.delete()
    start = start.getNext
    next  = start.getNext
  }
  start
}

def solve2(n: Int): Elf = {
  val elves = Elf.buildCircle(n)

  var start = elves.head
  var mid   = elves(n/2)
  for (i <- 0 until n-1) {
    if (i % 1000 == 0) println(s"step $i: $start <- $mid")
    mid.delete()
    mid = mid.getNext
    if ((n-i)%2 == 1) mid = mid.getNext
    start = start.getNext
  }
  start
}

println("--- test part #1 ---")
println(solve(5))
println()

println("--- test part #2 ---")
println(solve2(5))
println()

val input = 3004953
println("--- part #1 ---")
println(solve(input))

println("--- part #2 ---")
println(solve2(input))
