import scala.collection.mutable.ListBuffer

/**
  * Created by matt on 12/11/2016.
  */

trait Connection
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
  val input = ListBuffer[Int]()

  def addChip(n: Int): Unit = input += n

  def isFull: Boolean = input.length == 2

  def getLow:  Option[Int] = if (isFull) Some(input.min) else None
  def getHigh: Option[Int] = if (isFull) Some(input.max) else None

  override def toString(): String =
    s"Bot(low=$low high=$high input=$input)"
}

def initializeBots(instructions: List[String]) = {
  // parse instructions
  val instrValue ="""value (\d+) goes to bot (\d+)""".r
  val instrBot = """bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)""".r
  val bots = instructions.flatMap(s =>
    s match {
      case instrBot(n, typeLow, nLow, typeHigh, nHigh) =>
        Some(n.toInt -> Bot(makeBot(typeLow, nLow.toInt), makeBot(typeHigh, nHigh.toInt)))
      case _ => None
    }).toMap
  instructions.foreach(s =>
    s match {
      case instrValue(chip, bot) =>
        bots(bot.toInt).addChip(chip.toInt)
      case _ =>
    }
  )
  bots
}

def printBots(bots: Map[Int, Bot]) =
  for ((n, bot) <- bots)
    println(s"$n -> $bot")

val testInstructions = List(
  "value 5 goes to bot 2",
  "bot 2 gives low to bot 1 and high to bot 0",
  "value 3 goes to bot 1",
  "bot 1 gives low to output 1 and high to bot 0",
  "bot 0 gives low to output 2 and high to output 0",
  "value 2 goes to bot 2"
)
val test = initializeBots(testInstructions)
printBots(test)

//println(bots)
//for (i <- instructions.filter(s => "^bot".r.findFirstIn(s).isDefined))
//  println(i)
