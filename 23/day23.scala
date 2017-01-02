import scala.collection.mutable
import scala.io.Source

sealed trait Expression
case class Register(name: String) extends Expression
case class Literal(i: Int) extends Expression

sealed trait Instruction
case class Copy(e1: Expression, e2: Expression) extends Instruction
case class Inc(e: Expression) extends Instruction
case class Dec(e: Expression) extends Instruction
case class Jump(e1: Expression, e2: Expression) extends Instruction
case class Toggle(e: Expression) extends Instruction

object AsExpr {
  val number = "(-?[0-9]+)".r
  val register = "([a-z]+)".r

  def unapply(s: String): Option[Expression] = s match {
    case number(n)   => Some(Literal(n.toInt))
    case register(r) => Some(Register(r))
    case _ => None
  }
}

val copy   = s"cpy (\w+) (\w+)".r
val inc    = s"inc (\w+)".r
val dec    = s"dec (\w+)".r
val jump   = s"jnz (\w+) (\w+)".r

def parse(s: String): Instruction = s match {
  case copy(AsExpr(e1), AsExpr(e2)) => Copy(e1, e2)
  case inc(AsExpr(e))               => Inc(e)
  case dec(AsExpr(e))               => Dec(e)
  case jump(AsExpr(e1), AsExpr(e2)) => Jump(e1, e2)
}

class AssembunnyProgram(instructions: List[Instruction]) {
  var pointer = 0
  var register = new mutable.HashMap[String, Int]()
  var step = 0

  reset()

  def reset() = {
    step = 0
    pointer = 0
    register("a") = 0
    register("b") = 0
    register("c") = 1
    register("d") = 0
  }

  def getValue(v: Expression): Int = {
    v match {
      case Register(name) => register(name)
      case Literal(n) => n
    }
  }
}

class AssembunnyComputer {
  var pointer = 0
  val register = new mutable.HashMap[String, Int]()
  var step = 0


  def execute(instructions: Seq[Instruction]) = {
    reset()
    while (pointer < instructions.length) {
      val inst = instructions(pointer)
      inst match {
        case Copy(value, reg) =>
          register(reg.name) = getValue(value)
          pointer += 1
        case Inc(reg) =>
          register(reg.name) += 1
          pointer += 1
        case Dec(reg) =>
          register(reg.name) -= 1
          pointer += 1
        case Jump(value, num) =>
          if (getValue(value) != 0)
            pointer += num
          else
            pointer += 1
      }
      step += 1
      if ( step % 10000 == 0 ) printState()
    }
    printState()
  }

  def printState() = {
    println(s"step=$step pos=$pointer a=${register("a")} b=${register("b")} c=${register("c")} d=${register("d")}")
  }
}


val test = List(
  "cpy 41 a",
  "inc a",
  "inc a",
  "dec a",
  "jnz a 2",
  "dec a"
).map(parse)

//val input = Source.fromFile("input.txt").getLines.toList.map(parse)

val c = new AssembunnyComputer()
println("### test ###")
c.execute(test)
println()
//println("### input ###")
//c.execute(input)