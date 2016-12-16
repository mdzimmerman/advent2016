import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by mzimmerman on 12/9/16.
  */
object IPv7 {
  sealed trait State
  case object InExt extends State
  case object InInt extends State

  def parse(s: String): IPv7 = parse(s, IPv7(), InExt, "")

  @tailrec
  def parse(s: String, ip: IPv7, state: State, buffer: String): IPv7 = {
    if (s.isEmpty) {
      ip.appendExt(buffer)
    } else {
      state match {
        case InExt =>
          s.head match {
            case '[' => parse(s.tail, ip.appendExt(buffer), InInt, "")
            case c => parse(s.tail, ip, InExt, buffer + c)
          }
        case InInt =>
          s.head match {
            case ']' => parse(s.tail, ip.appendInt(buffer), InExt, "")
            case c => parse(s.tail, ip, InInt, buffer + c)
          }
      }
    }
  }
}

case class IPv7(ext: List[String] = List(), int: List[String] = List()) {
  def appendExt(s: String): IPv7 = {
    if (s.isEmpty)
      this.copy()
    else
      IPv7(ext ::: List(s), int)

  }

  def appendInt(s: String): IPv7 = {
    if (s.isEmpty)
      this.copy()
    else
      IPv7(ext, int ::: List(s))
  }

  private def isABBA(s: String): Boolean =
    s.sliding(4).map(sub => {
      val c = sub.toSeq
      c.length >= 4 && c(0) == c(3) && c(1) == c(2) && c(0) != c(1)
    }).exists(x => x)

  private def isExtABBA(): Boolean = ext.exists(isABBA)

  private def isIntABBA(): Boolean = int.exists(isABBA)

  def isTLS(): Boolean = isExtABBA && !isIntABBA

  private def getABA(s: String): List[String] =
  s.sliding(3).filter(ss => {
    val c = ss.toSeq
    c(0) == c(2) && c(0) != c(1)
  }).toList

  private def matchingABA(s1: String, s2: String): Boolean = {
    val c1 = s1.toSeq
    val c2 = s2.toSeq
    c1.length == 3 && c2.length == 3 &&
      c1(0) == c1(2) && c1(0) == c2(1) &&
      c2(0) == c2(2) && c2(0) == c1(1)
  }

  def isSSL(): Boolean = {
    val extABA = ext.flatMap(getABA)
    val intABA = int.flatMap(getABA)
    (for (e <- extABA; i <- intABA) yield (e, i)).exists(p => matchingABA(p._1, p._2))
  }
}

println("--- test isTLS ---")
val test = List(
  "abba[mnop]qrst",
  "abcd[bddb]xyyx",
  "aaaa[qwer]tyui",
  "ioxxoj[asdfgh]zxcvbn"
)
test.foreach(s => {
  val ip = IPv7.parse(s)
  println(s"$ip isTLS=${ip.isTLS}")
})
println()

println("--- test isSSL ---")
val test2 = List(
  "aba[bab]xyz",
  "xyx[xyx]xyx",
  "aaa[kek]eke",
  "zazbz[bzb]cdb"
)
test2.foreach(s => {
  val ip = IPv7.parse(s)
  println(s"$ip isSSL=${ip.isSSL}")
})
println()

val input = Source.fromFile("input.txt").getLines.map(IPv7.parse).toList
println(s"count = ${input.length}")
println(s"--- Part 1 ---")
println(s"isTLS = ${input.count(_.isTLS)}")
println(s"--- Part 2 ---")
println(s"isSSL = ${input.count(_.isSSL)}")
