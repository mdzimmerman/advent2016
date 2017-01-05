import java.math.BigInteger
import java.security.MessageDigest

import scala.annotation.tailrec

/**
  * Created by mzimmerman on 12/16/16.
  */

case class MessageKey(i: Long, key: String, group3: Char, groups5: Set[Char])

class MessageKeyLookup(val prefix: String, stretch: Int = 1) {
  var numCached: Long = -1L
  val cache = new scala.collection.mutable.HashMap[Long, MessageKey]()

  private val m = MessageDigest.getInstance("MD5")

  def md5(s: String): String = {
    m.reset()
    m.update(s.getBytes())
    val digest = m.digest()
    val bigInt = new BigInteger(1, digest)
    "%032x".format(bigInt)
  }

  @tailrec
  final def md5stretch(s: String, n: Int): String = {
    if (n <= 0)
      s
    else {
      val e = md5(s)
      md5stretch(e, n - 1)
    }
  }


  def firstGroupOfN(s: String, n: Int): Option[Char] = {
    s.sliding(n).foreach{ x =>
      val xh = x.head
      if (x.forall(xh == _)) return Some(xh)
    }
    None
  }
  
  def groupsOfN(s: String, n: Int): Set[Char] = {
    s.sliding(n).flatMap { x =>
      val xh = x.head
      if (x.forall(xh == _))
        Some(xh)
      else
        None
    }.toSet
  }

  def populateTo(n: Long): Unit = {
    if (n > numCached) {
      println(s"## populate ${numCached+1}-$n")
      for (i <- numCached+1 to n) {
        val s = prefix + i.toString
        val key = md5stretch(s, stretch)
        val n3 = firstGroupOfN(key, 3)
        val n5 = groupsOfN(key, 5)
        if (n3.isDefined)
          cache += i -> new MessageKey(i, key, n3.get, n5)
      }
      numCached = n
    }
  }

  def get(n: Long): Option[MessageKey] = {
    populateTo(n)
    cache.get(n)
  }

  def findMatchingKey(i: Long, within: Long): Option[MessageKey] = {
    populateTo(i+within)
    get(i) match {
      case Some(m) =>
        val c = m.group3
        for (j <- i+1 to i+within) {
          get(j) match {
            case Some(m2) => if (m2.groups5.contains(c)) return Some(m2)
            case None =>
          }
        }
      case None =>
    }
    None
  }

  def findNKeys(n: Int): Unit = {
    var i = 0
    var foundKeys = 0
    while (foundKeys < n) {
      get(i) match {
        case Some(m) =>
          findMatchingKey(i, 1000L) match {
            case Some(m2) =>
              foundKeys += 1
              println(s"$foundKeys: $m -> $m2")
            case None =>
          }
        case None =>
      }
      i += 1
      //if (i % 10 == 0) println(s"## $i")
    }
  }

  def printCache(): Unit = {
    for (i <- 0L to numCached) {
      get(i) match {
        case Some(m) => println(s"$i => $m")
        case None =>
      }
    }
  }
}

/*
println("--- test ---")
val testLookup = new MessageKeyLookup("abc")
testLookup.findNKeys(64)
println()

println("--- test #2 ---")
val testLookup2 = new MessageKeyLookup("abc", 2017)
println(testLookup2.md5stretch("abc0", 1))
println(testLookup2.md5stretch("abc0", 2))
println(testLookup2.md5stretch("abc0", 3))
println("...")
println(testLookup2.md5stretch("abc0", 2017))
println()

println("--- test #3 ---")
testLookup2.findNKeys(64)

println("--- part #1 ---")
val inputLookup = new MessageKeyLookup("yjdafjpo")
inputLookup.findNKeys(64)
*/

println("--- part #2 ---")
val inputLookup2 = new MessageKeyLookup("yjdafjpo", 2017)
inputLookup2.findNKeys(65)