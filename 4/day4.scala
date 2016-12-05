import scala.annotation.tailrec
import scala.io.Source

/**
  * Created by mzimmerman on 12/5/16.
  */

case class RoomName(encrypted: String, sectorId: Int, checksum: String) {
  type Bins = Map[Char, Int]
  type InvertedBins = Map[Int, List[Char]]

  @tailrec
  final def calculateHist(cs: List[Char], bins: Bins): Bins = {
    if (cs.isEmpty) {
      bins
    } else {
      val c = cs.head
      val newBins = if (c == '-') {
        bins
      } else {
        bins + (c -> (bins.getOrElse(c, 0)+1))
      }
      calculateHist(cs.tail, newBins)
    }
  }

  @tailrec
  final def invertHist(bins: Bins, iBins: InvertedBins): InvertedBins = {
    if (bins.isEmpty) {
      iBins
    } else {
      val (k, v) = bins.head
      val list: List[Char] = (iBins.getOrElse(v, List()) ::: List(k)).sorted
      val iBinsNew: InvertedBins = iBins + (v -> list)
      invertHist(bins.tail, iBinsNew)
    }
  }

  def validateChecksum(): Boolean = {
    val hist = calculateHist(encrypted.toList, Map[Char, Int]())
    val hist2 = invertHist(hist, Map())
    val realChecksum = hist2.keys.toList.sorted.reverse.flatMap(hist2.get(_)).flatten.take(5).mkString("")
    //println(realChecksum)
    checksum == realChecksum
  }

  override def toString(): String = {
    s"RoomName(encrypted=$encrypted, sectorId=$sectorId, checksum=$checksum)"
  }
}

object RoomNameString {
  val RoomNameRegex = "(.+)-(\\d+)\\[(.+)\\]".r

  object AsInt {
    def unapply(s: String) =
      try {
        Some(s.toInt)
      } catch {
        case e: NumberFormatException => None
      }
  }

  def unapply(s: String): Option[RoomName] = s match {
    case RoomNameRegex(name, AsInt(sectorId), checksum) => Some(RoomName(name, sectorId, checksum))
    case _ => None
  }
}

def sumRooms(input: List[String]): Int = {
  input.flatMap(_ match {
    case RoomNameString(p) => Some(p)
    case _ => None
  }).filter(_.validateChecksum()).map(_.sectorId).sum
}

val test = List(
  "aaaaa-bbb-z-y-x-123[abxyz]",
  "a-b-c-d-e-f-g-h-987[abcde]",
  "not-a-real-room-404[oarel]",
  "totally-real-room-200[decoy]"
)
println(s"test  = ${sumRooms(test)}")

val input = Source.fromFile("input.txt").getLines.toList
println(s"input = ${sumRooms(input)}")

