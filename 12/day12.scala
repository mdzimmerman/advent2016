import scala.collection.mutable
import scala.io.Source

sealed trait Value
case class Register(name: String) extends Value
case class Number(i: Int) extends Value

sealed trait Instruction
case class Copy(value: Value, register: Register) extends Instruction
case class Inc(register: Register)                extends Instruction
case class Dec(register: Register)                extends Instruction
case class Jump(value: Value, n: Int)             extends Instruction

val number = "-?[0-9]+"
val copyNumber   = s"cpy ($number) ([a-z])".r
val copyRegister = s"cpy ([a-z]) ([a-z])".r
val inc          = s"inc ([a-z])".r
val dec          = s"dec ([a-z])".r
val jumpNumber   = s"jnz ($number) ($number)".r
val jumpRegister = s"jnz ([a-z]) ($number)".r

def parse(s: String): Instruction = s match {
  case copyNumber(n, r)     => Copy(Number(n.toInt), Register(r))
  case copyRegister(r1, r2) => Copy(Register(r1), Register(r2))
  case inc(r)               => Inc(Register(r))
  case dec(r)               => Dec(Register(r))
  case jumpNumber(n1, n2)   => Jump(Number(n1.toInt), n2.toInt)
  case jumpRegister(r, n)   => Jump(Register(r), n.toInt)
}

class AssembunnyComputer {
  var pointer = 0
  val register = new mutable.HashMap[String, Int]()
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

  def getValue(v: Value): Int = {
    v match {
      case Register(name) => register(name)
      case Number(n) => n
    }
  }

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

val input = Source.fromFile("input.txt").getLines.toList.map(parse)

val c = new AssembunnyComputer()
println("### test ###")
c.execute(test)
println()
println("### input ###")
c.execute(input)