import java.math.BigInteger
import java.security.MessageDigest

import scala.annotation.tailrec

/**
  * Created by mzimmerman on 12/5/16.
  */


val m = MessageDigest.getInstance("MD5")

case class PasswordChar(c1: Char, c2: Char)

def md5Zeros(s: String): Option[PasswordChar] = {
  m.reset()
  m.update(s.getBytes())
  val digest = m.digest()
  val bigInt = new BigInteger(1, digest)
  val hashText = bigInt.toString(16)
  val nZeros = 32 - hashText.length
  if (nZeros < 5)
    None
  else if (nZeros == 5)
    Some(PasswordChar(hashText.charAt(0), hashText.charAt(1)))
  else if (nZeros == 6)
    Some(PasswordChar('0', hashText.charAt(0)))
  else // nZeros >= 7
    Some(PasswordChar('0', '0'))
}

@tailrec
def calcPassword(prefix: String, i: Int, password: List[Char]): List[Char] = {
  if (password.length >= 8) {
    password
  } else {
    val passwordNew: List[Char] = md5Zeros(prefix + i.toString) match {
      case Some(p) => {
        println(s"prefix=$prefix i=$i char=$p")
        password ::: List(p.c1)
      }
      case None => password
    }
    calcPassword(prefix, i+1, passwordNew)
  }
}

val test = List(
  "abc",
  "abc3231929",
  "abc5017308",
  "abc5278568"
)

test.foreach(s => {
  println(s"$s => ${md5Zeros(s)}")
})

println(calcPassword("abc", 0, List()).mkString(""))
println(calcPassword("reyedfim", 0, List()).mkString(""))