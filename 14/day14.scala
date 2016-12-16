import java.math.BigInteger
import java.security.MessageDigest

/**
  * Created by mzimmerman on 12/16/16.
  */

val m = MessageDigest.getInstance("MD5")

def md5(s: String): String = {
  m.update(s.getBytes())
  val digest = m.digest()
  val bigInt = new BigInteger(1, digest)
  bigInt.toString(16)
}

val prefix = "abc"
for (i <- 0 to 20) {
  val key = prefix + i.toString
  println(s"$key => ${md5(key)}")
}

