import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

sealed trait Value
case class Register(name: String) extends Value
case class Number(i: Int) extends Value

sealed trait Instruction
case class Copy(value: Value, register: Register) extends Instruction
case class Inc(register: Register)                extends Instruction
case class Dec(register: Register)                extends Instruction
case class Jump(value: Value, n: Int)             extends Instruction
case class Out(value: Value)                      extends Instruction

val letter = "[a-z]"
val number = "-?[0-9]+"
val copyNumber   = s"cpy ($number) ($letter)".r
val copyRegister = s"cpy ($letter) ($letter)".r
val inc          = s"inc ($letter)".r
val dec          = s"dec ($letter)".r
val jumpNumber   = s"jnz ($number) ($number)".r
val jumpRegister = s"jnz ($letter) ($number)".r
val outNumber    = s"out ($number)".r
val outRegister  = s"out ($letter)".r

def parse(s: String): Instruction = s match {
  case copyNumber(n, r)     => Copy(Number(n.toInt), Register(r))
  case copyRegister(r1, r2) => Copy(Register(r1), Register(r2))
  case inc(r)               => Inc(Register(r))
  case dec(r)               => Dec(Register(r))
  case jumpNumber(n1, n2)   => Jump(Number(n1.toInt), n2.toInt)
  case jumpRegister(r, n)   => Jump(Register(r), n.toInt)
  case outNumber(n)         => Out(Number(n.toInt))
  case outRegister(r)       => Out(Register(r))
  case _ => throw new Exception("Invalid instruction")
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
    register("c") = 0
    register("d") = 0
  }

  def setRegister(name: String, value: Int) = {
    register(name) = value
  }

  def getValue(v: Value): Int = {
    v match {
      case Register(name) => register(name)
      case Number(n) => n
    }
  }

  def execute(instructions: Seq[Instruction]): Seq[Int] = {
    val buffer = new ListBuffer[Int]()
    while (pointer < instructions.length && buffer.length < 32) {
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
        case Out(value) =>
          buffer.append(getValue(value))
          pointer += 1
      }
      step += 1
    }
    buffer
  }

  def printState() = {
    println(s"step=$step pos=$pointer a=${register("a")} b=${register("b")} c=${register("c")} d=${register("d")}")
  }
}

val input = Source.fromFile("input.txt").getLines.toList.map(parse)

def checkOut(out: Seq[Int]): Boolean = {
  out.sliding(2).forall(s => s(0) != s(1))
}

val seqs = List(
  Seq(0, 1, 0, 1, 0, 1, 0, 1),
  Seq(1, 0, 1, 0, 1, 0, 1, 0),
  Seq(1, 0, 1, 0, 0, 1, 1, 0)
)
for (s <- seqs) {
  println(s"${s.mkString("")} => ${checkOut(s)}")
}

val c = new AssembunnyComputer()
var a = 1
var stop = false
while (!stop) {
  c.reset()
  c.setRegister("a", a)
  val out = c.execute(input)
  println(s"""$a => ${out.mkString("")} => ${checkOut(out)}""")
  stop = checkOut(out)
  a += 1
}
