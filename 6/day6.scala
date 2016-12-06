import scala.collection.mutable
import scala.io.Source

/**
  * Created by mzimmerman on 12/6/16.
  */

type Hist = mutable.Map[Char, Int]

def calcHist(in: List[String]): Array[Hist] = {

  val hist: Array[Hist] = (0 until in.head.length).map(_ => new mutable.HashMap[Char, Int]()).toArray

  for (line <- in) {
    for ((c, i) <- line.toList.view.zipWithIndex) {
      val count = hist(i).getOrElse(c, 0)
      hist(i).put(c, count+1)
    }
  }
  hist.foreach(println)
  hist
}

def getMostCommon(h: Hist): Char = {
  var max = ('*', 0)

  for ((char, count) <- h) {
    if (count > max._2) {
        max = (char, count)
      }
  }
  max._1
}

def getLeastCommon(h: Hist): Char = {
  var min = ('*', 1000)

  for ((char, count) <- h) {
    if (count < min._2) {
      min = (char, count)
    }
  }
  min._1
}

val test  = Source.fromFile("test.txt").getLines.toList
val input = Source.fromFile("input.txt").getLines.toList

println(calcHist(test).toList.map(getMostCommon).mkString(""))
println(calcHist(input).toList.map(getMostCommon).mkString(""))

println(calcHist(test).toList.map(getLeastCommon).mkString(""))
println(calcHist(input).toList.map(getLeastCommon).mkString(""))