import scala.annotation.tailrec

/**
  * Created by mzimmerman on 12/9/16.
  */

sealed trait State
case object InExt extends State
case object InInt extends State

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
}

val bracketed = """\[(.+)\].*""".r

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
          case c   => parse(s.tail, ip, InExt, buffer + c)
        }
      case InInt =>
        s.head match {
          case ']' => parse(s.tail, ip.appendInt(buffer), InExt, "")
          case c   => parse(s.tail, ip, InInt, buffer + c)
        }
    }
  }
}

val test = List(
  "abba[mnop]qrst",
  "abcd[bddb]xyyx",
  "aaaa[qwer]tyui",
  "ioxxoj[asdfgh]zxcvbn"
)

test.foreach(s => println(s"$s => ${parse(s)}"))

