trait Item

case class  Gen(n: Int) extends Item
case class Chip(n: Int) extends Item

case class State(elevator: Int, floor0: Set[Item], floor1: Set[Item], floor2: Set[Item], floor3: Set[Item]) {
  val floors = Array(floor0, floor1, floor2, floor3)

  def printState = {
    for (i <- 3 to 0 by -1) {
      val e = if (i == elevator) "E" else "."
      println(s"F$i $e ${floors(i)}")
    }
  }

  def floorIsValid(floor: Set[Item]): Boolean = {
    val chips = floor.filter(_.isInstanceOf[Chip]).map(_.asInstanceOf[Chip])
    val gens  = floor.filter(_.isInstanceOf[Gen]).map(_.asInstanceOf[Gen])
    if (chips.isEmpty)
      true
    else
      chips.forall( c => { gens.contains(Gen(c.n)) || gens.isEmpty } )
  }

  def getNextDir(dir: Int): Seq[State] = {
    val moving = floors(elevator).subsets.toSeq.filter(s => s.size == 1 || s.size == 2)
    moving.flatMap(s => {
      val elevatorNew = elevator+dir
      val floorsNew = floors.clone()
      floorsNew(elevator)    = floors(elevator) -- s
      floorsNew(elevatorNew) = floors(elevatorNew) ++ s
      if ( floorIsValid(floorsNew(elevator)) && floorIsValid(floorsNew(elevatorNew)))
        Some(State(elevatorNew, floorsNew(0), floorsNew(1), floorsNew(2), floorsNew(3)))
      else
        None
    })
  }

  def getNext(): Seq[State] = {
    val nextDown = if (elevator == 0) Seq() else getNextDir(-1)
    val nextUp   = if (elevator == 3) Seq() else getNextDir(1)
    nextUp ++ nextDown
  }

  def finished(): Boolean = {
    floor0.isEmpty && floor1.isEmpty && floor2.isEmpty
  }
}

def bfs(start: State): Option[Int] = {
  var step = 0
  val visited = new scala.collection.mutable.HashSet[State]()
  val queue   = new scala.collection.mutable.Queue[(State, Int)]()
  visited.add(start)
  queue.enqueue((start, 0))
  while (queue.nonEmpty) {
    step += 1
    val (s, length) = queue.dequeue()
    if (step % 10000 == 0) println(s"$step: $length -> $s")
    for (sNext <- s.getNext) {
      if (sNext.finished) {
        println(s"${length + 1} -> $sNext")
        return Some(length + 1)
      } else if (!visited.contains(sNext)) {
        visited.add(sNext)
        queue.enqueue((sNext, length+1))
      }
    }
  }
  None
}

val test0 = new State(
  0,
  Set(Chip(0), Chip(1)),
  Set(Gen(0)),
  Set(Gen(1)),
  Set()
)

//println(bfs(test0))

val input0 = new State(
  0,
  Set(Gen(0), Gen(1), Chip(1), Gen(2), Gen(3), Chip(3), Gen(4), Chip(4)),
  Set(Chip(0), Chip(2)),
  Set(),
  Set()
)

//println(bfs(input0))

val input1 = new State(
  0,
  Set(Gen(0), Gen(1), Chip(1), Gen(2), Gen(3), Chip(3), Gen(4), Chip(4), Gen(5), Chip(5), Gen(6), Chip(6)),
  Set(Chip(0), Chip(2)),
  Set(),
  Set()
)

println(bfs(input1))

val input2 = new State(
  0,
  Set(Gen(0), Chip(0), Gen(1), Chip(1)),
  Set(),
  Set(),
  Set()
)

//println(bfs(input2))

//println(test0)
//println(test0)
//for (s <- test0.getNext()) {
  //println("  "+s)
  //for (s2 <- s.getNext()) {
    //println("    "+s2)
  //}
//}



