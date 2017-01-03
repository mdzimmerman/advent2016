import scala.collection.mutable
import scala.io.Source

sealed trait Expression
case class Register(name: String) extends Expression
case class Literal(i: Int) extends Expression

sealed trait Instruction {
  def evaluate(a: AssembunnyProgram): Unit
  def toggle(): Instruction
}
case class Copy(e1: Expression, e2: Expression) extends Instruction {
  def evaluate(a: AssembunnyProgram): Unit = {
    e2 match {
      case r: Register =>
        a.setRegister(r.name, a.getValue(e1))
      case l: Literal =>
        // invalid copy, do nothing
    }
    a.pointer += 1
  }
  def toggle(): Instruction = new Jump(e1, e2)
}
case class Inc(e: Expression) extends Instruction {
  def evaluate(a: AssembunnyProgram): Unit = {
    e match {
      case r: Register =>
        a.setRegister(r.name, a.getValue(r)+1)
      case l: Literal =>
        // invalid inc, do nothing
    }
    a.pointer += 1
  }
  def toggle(): Instruction = new Dec(e)
}
case class Dec(e: Expression) extends Instruction {
  def evaluate(a: AssembunnyProgram): Unit = {
    e match {
      case r: Register =>
        a.setRegister(r.name, a.getValue(r)-1)
      case l: Literal =>
        // invalid dec, do nothing
    }
    a.pointer += 1
  }
  def toggle(): Instruction = new Inc(e)
}
case class Jump(e1: Expression, e2: Expression) extends Instruction {
  def evaluate(a: AssembunnyProgram): Unit = {
    if (a.getValue(e1) != 0)
      a.pointer += a.getValue(e2)
    else
      a.pointer += 1
  }
  def toggle(): Instruction = new Copy(e1, e2)
}
case class Toggle(e: Expression) extends Instruction {
  def evaluate(a: AssembunnyProgram): Unit = {
    val i = a.pointer + a.getValue(e)
    if (i >= 0 && i < a.instructions.length)
      a.instructions(i) = a.instructions(i).toggle()
    a.pointer += 1  // no-op
  }
  def toggle(): Instruction = new Inc(e)
}

object AsExpr {
  val number = "(-?[0-9]+)".r
  val register = "([a-z]+)".r

  def unapply(s: String): Option[Expression] = s match {
    case number(n)   => Some(Literal(n.toInt))
    case register(r) => Some(Register(r))
    case _ => None
  }
}

val copy   = s"cpy (.+) (.+)".r
val inc    = s"inc (.+)".r
val dec    = s"dec (.+)".r
val jump   = s"jnz (.+) (.+)".r
val tgl    = s"tgl (.+)".r

def parse(s: String): Option[Instruction] = s match {
  case copy(AsExpr(e1), AsExpr(e2)) => Some(Copy(e1, e2))
  case inc(AsExpr(e))               => Some(Inc(e))
  case dec(AsExpr(e))               => Some(Dec(e))
  case jump(AsExpr(e1), AsExpr(e2)) => Some(Jump(e1, e2))
  case tgl(AsExpr(e))               => Some(Toggle(e))
  case _ => {
    //throw new Exception("invalid instruction")
    None
  }
}

def compile(s: List[String], debug: Int = 0): AssembunnyProgram = {
  val instructions = s.flatMap(parse).toArray
  return new AssembunnyProgram(instructions, debug)
}

class AssembunnyProgram(val instructions: Array[Instruction], debug: Int = 0) {
  var pointer = 0
  var register = new mutable.HashMap[String, Int]()
  var step = 0
  reset()

  def reset() = {
    step = 0
    pointer = 0
    register("a") = 0
    register("b") = 0
    register("c") = 0
    register("d") = 0
  }

  def getRegister(name: String): Int = register(name)

  def setRegister(name: String, v: Int) = {
    register(name) = v
  }

  def getValue(e: Expression): Int = {
    e match {
      case Register(name) => getRegister(name)
      case Literal(n) => n
    }
  }

  def execute() = {
    while (pointer < instructions.length) {
      val inst = instructions(pointer)
      inst.evaluate(this)
      step += 1
      if ( debug > 0 && (step % debug) == 0 ) printState()
    }
  }

  def printState() = {
    println(s"step=$step pos=$pointer a=${register("a")} b=${register("b")} c=${register("c")} d=${register("d")}")
  }
}


val test = compile(List(
  "cpy 2 a",
  "tgl a",
  "tgl a",
  "tgl a",
  "cpy 1 a",
  "dec a",
  "dec a"
), debug=1)



println("--- test ---")
test.execute()
println()

println("--- input (part 1) ---")
val input = compile(Source.fromFile("input.txt").getLines.toList, debug=10000)
input.setRegister("a", 7)
input.execute()
input.printState()
println(s"out = ${input.getRegister("a")}")
println()

println("--- input (part 2) ---")
val input2 = compile(Source.fromFile("input.txt").getLines.toList, debug=10000)
input2.setRegister("a", 12)
input2.execute()
input2.printState()
println(s"out = ${input2.getRegister("a")}")


//println("### input ###")
//c.execute(input)