class Exchange(n: Int, mode: String = "next", debug: Boolean = false) {
  val presents = Array.fill[Int](n)(1)

  def countWithPresents(): Int = presents.count(_ > 0)

  def next(i: Int): Int = if (i == n-1) 0 else i+1

  def nextWithPresents(i0: Int): Int = {
    var i = next(i0)
    while (presents(i) == 0)
      i = next(i)
    i
  }

  def nextTake(i: Int): Int = {
    val before = (0 until i).filter(presents(_) > 0)
    val after  = (i+1 until n).filter(presents(_) > 0)
    val others = after
    val take = others.length/2 - 1 + others.length%2
    others(take)
  }

  def offset(i: Int): Int = i+1

  def run(): Int = {
    var t = 0
    var i = 0
    var done = false
    while (true) {
      val iNext = nextWithPresents(i)
      if (i == iNext) return offset(i)
      if (presents(i) > 0) {
        val iTake = if (mode == "next") iNext else nextTake(i)
        if (debug) println(s"${offset(i)}: takes ${presents(iTake)} present(s) from ${offset(iTake)}")
        presents(i) += presents(iTake)
        presents(iTake) = 0
      }
      else {
        if (debug) println(s"${i+1}: skipped")
      }
      if (t % 1000 == 0)
        println(t)
      t += 1
      i = iNext
    }
    offset(i) // change to 1-offset (should never get here)
  }
}

println("--- test next ---")
val test = new Exchange(5, debug=true)
println(test.run())
println()

println("--- test across ---")
//val test2 = new Exchange(5, mode="across", debug=true)
//println(test2.run())
println()

println("--- input next ---")
val input = new Exchange(3004953)
println(input.run())
println()

println("--- input across ---")
val input2 = new Exchange(3004953, mode="across")
println(input2.run())