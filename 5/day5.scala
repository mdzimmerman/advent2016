import java.math.BigInteger
import java.security.MessageDigest

/**
  * Created by mzimmerman on 12/5/16.
  */

val prefix = "abc3231929"

val m = MessageDigest.getInstance("MD5")
m.reset();
m.update(prefix.getBytes())
val digest = m.digest()
val bigInt = new BigInteger(1, digest)
val hashText = bigInt.toString(16)

println(hashText)
