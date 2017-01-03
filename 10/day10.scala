import scala.collection.mutable
import scala.io.Source

/**
  * Created by matt on 12/11/2016.
  */

sealed trait Connection
case class BotConnection(n: Int) extends Connection
case class OutConnection(n: Int) extends Connection

def makeBot(t: String, n: Int): Connection = {
  if (t == "output")
    OutConnection(n)
  else if (t == "bot")
    BotConnection(n)
  else
    throw new Exception("bad input")
}

case class Bot(val low: Connection, val high: Connection) {
  val input = mutable.ListBuffer[Int]()
  var hasRun: Boolean = false

  def addChip(n: Int): Unit = input += n

  def isFull: Boolean = input.length == 2

  def getLowChip:  Option[Int] = if (isFull) Some(input.min) else None
  def getHighChip: Option[Int] = if (isFull) Some(input.max) else None

  override def toString(): String =
    s"Bot(low=$low high=$high input=$input)"
}

def initializeBots(instructions: List[String]) = {
  // parse instructions
  val instrValue ="""value (\d+) goes to bot (\d+)""".r
  val instrBot = """bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)""".r
  val bots = instructions.flatMap{
    case instrBot(n, typeLow, nLow, typeHigh, nHigh) =>
      Some(n.toInt -> Bot(makeBot(typeLow, nLow.toInt), makeBot(typeHigh, nHigh.toInt)))
    case _ => None
  }.toMap
  instructions.foreach {
    case instrValue(chip, bot) =>
      bots(bot.toInt).addChip(chip.toInt)
    case _ =>
  }
  bots
}

def printBots(bots: Map[Int, Bot]) =
  for (n <- bots.keys.toList.sorted)
    println(s"$n -> ${bots(n)}")

def printOut(out: Map[Int, Int]) =
  for (n <- out.keys.toList.sorted)
    println(s"$n -> ${out(n)}")

def getBotsToCheck(bots: Map[Int, Bot]) =
  bots.filter(b => b._2.isFull && !b._2.hasRun).keys.toList

def runBots(bots: Map[Int, Bot]): Map[Int, Int] = {
  val out = mutable.HashMap[Int, Int]()
  var check = getBotsToCheck(bots)
  while (check.nonEmpty) {
    println(check)
    for (i <- check) {
      val current = bots(i)
      current.hasRun = true
      current.low match {
        case BotConnection(n) => bots(n).addChip(current.getLowChip.get)
        case OutConnection(n) => out(n) = current.getLowChip.get
      }
      current.high match {
        case BotConnection(n) => bots(n).addChip(current.getHighChip.get)
        case OutConnection(n) => out(n) = current.getHighChip.get
      }
    }
    check = getBotsToCheck(bots)
  }
  out.toMap // make immutable
}

println("--- test ---")
val test = List(
  "value 5 goes to bot 2",
  "bot 2 gives low to bot 1 and high to bot 0",
  "value 3 goes to bot 1",
  "bot 1 gives low to output 1 and high to bot 0",
  "bot 0 gives low to output 2 and high to output 0",
  "value 2 goes to bot 2"
)
val testBots = initializeBots(test)
val testOut = runBots(testBots)
printBots(testBots)
printOut(testOut)
println()

println("--- input ---")
val input = Source.fromFile("input.txt").getLines().toList
val inputBots = initializeBots(input)
val inputOut = runBots(inputBots)
println("# bots #")
printBots(inputBots)
println("# outputs #")
printOut(inputOut)
println()

println("--- part #1 ---")
val part1 = inputBots.filter{b => b._2.getLowChip.get == 17 && b._2.getHighChip.get == 61}.keys.head
println(s"bot with 17 and 61 = ${part1}")
println()

println("--- part #2 ---")
println(s"mult chips in bins 0 (${inputOut(0)}), 1 (${inputOut(1)}), 2 (${inputOut(2)}) = ${inputOut(0) * inputOut(1) * inputOut(2)}")
//println(bots)
//for (i <- instructions.filter(s => "^bot".r.findFirstIn(s).isDefined))
//  println(i)
