import scala.collection.mutable

case class Cell(x: Int, y: Int, reachable: Boolean) {
  var parent: Option[Cell] = None
  //var g = 0
  var h = 0
  //def f: Int = g + h
}

case class Maze(xMax: Int,
                yMax: Int,
                input: Int,
                xStart: Int,
                yStart: Int,
                xEnd: Int,
                yEnd: Int) {
  val maze = Array.ofDim[Cell](yMax, xMax)
  val opened = new mutable.PriorityQueue[Cell]()(Ordering.by(c => c.h))
  val closed = new mutable.HashSet[Cell]()
  /*
  val wasHere = Array.ofDim[Boolean](yMax, xMax)
  val correctPath = Array.ofDim[Boolean](yMax, xMax)
  */

  for (j <- 0 until yMax; i <- 0 until xMax) {
    maze(j)(i) = Cell(i, j, getOpen(i, j))
  }
  val start = getCell(xStart, yStart).get
  val end   = getCell(xEnd, yEnd).get

  def getCell(x: Int, y: Int): Option[Cell] =
    if (x >= 0 && x < xMax && y >= 0 && y < yMax)
      Some(maze(y)(x))
    else
      None

  def getH(cell: Cell) = 10 * (Math.abs(cell.x - end.x) + Math.abs(cell.y - end.y))

  def getAdjacentCells(cell: Cell): List[Cell] =
    List(
      getCell(cell.x-1, cell.y),
      getCell(cell.x, cell.y-1),
      getCell(cell.x+1, cell.y),
      getCell(cell.x, cell.y+1)).flatten

  def updateCell(adj: Cell, cell: Cell): Unit = {
    //adj.g = cell.g + 10
    adj.h = getH(adj)
    adj.parent = Some(cell)
  }

  def countPath(cell: Cell): Int = countPath(cell, 0)

  def countPath(cell: Cell, length: Int): Int = {
    cell.parent match {
      case Some(p) => countPath(p, length + 1)
      case None => length
    }
  }

  def getPath(cell: Cell, path: List[Cell]): List[Cell] = {
    cell.parent match {
      case Some(p) => getPath(p, path ::: List(p))
      case None => path
    }
  }

  def printPath(): Unit = {
    var path = getPath(end, List())
    for (c <- path)
      println(c)
  }

  def solveAstar(): Boolean = {
    var possible = 0
    opened.enqueue(start)
    while (opened.nonEmpty) {
      val c = opened.dequeue()
      val pathLength = countPath(c)
      println(s"$c h=${c.h} length=$pathLength")
      if (pathLength <= 50) {
        possible += 1
      }
      closed.add(c)
      if (c == end) {
        val pathLength = getPath(end, List()).length
        println(s"length = $pathLength")
        println(s"possible = $possible")
        //printPath()
        return true
      }
      val adjCells = getAdjacentCells(c)
      for (adj <- adjCells) {
        if (adj.reachable && !closed.contains(adj)) {
          if (opened.toList.contains(adj)) {
            //if (adj.g > c.g + 10)
            //  updateCell(adj, c)
          } else {
            updateCell(adj, c)
            opened.enqueue(adj)
          }
        }
      }
    }
    false
  }

  /*
  def solveAlternate(): Boolean = {
    opened.clear()
    opened.enqueue(start)
    closed.clear()
    while (opened.nonEmpty) {
      val c = opened.dequeue()
      closed.add(c)
      if (c == end) {
        printPath()
      }

    }
  }
  */

  /*
  def solve(xStart: Int, yStart: Int, xEnd: Int, yEnd: Int): Boolean = {
    for (j <- 0 until yMax; i <- 0 until xMax) {
      wasHere(j)(i) = false
      correctPath(j)(i) = false
    }
    solveStep(xStart, yStart, xEnd, yEnd)
  }

  def solveStep(x: Int, y: Int, xEnd: Int, yEnd: Int): Boolean = {
    if (x == xEnd && y == yEnd) return true
    if (maze(y)(x) || wasHere(y)(x)) return false
    wasHere(y)(x) = true
    if (x != 0)
      if (solveStep(x-1, y, xEnd, yEnd)) {
        correctPath(y)(x) = true
        return true
      }
    if (x != xMax - 1)
      if (solveStep(x+1, y, xEnd, yEnd)) {
        correctPath(y)(x) = true
        return true
      }
    if (y != 0)
      if (solveStep(x, y-1, xEnd, yEnd)) {
        correctPath(y)(x) = true
        return true
      }
    if (y != yMax - 1)
      if (solveStep(x, y+1, xEnd, yEnd)) {
        correctPath(y)(x) = true
        return true
      }
    return false
  }
  */

  def getOpen(x: Int, y: Int): Boolean = {
    val sum = x*x + 3*x + 2*x*y + y + y*y + input
    val digits = sum.toBinaryString.toList.count(_ == '1')
    digits % 2 == 0
  }

  def show(): Unit = {
    val path = getPath(end, List())
    print("  ")
    for (i <- 0 until xMax) {
      //print(" ")
      print(i % 10)
    }
    println()
    for (j <- 0 until yMax) {
      print(j % 10)
      print(" ")
      for (i <- 0 until xMax) {
        val c = getCell(i, j).get
        if (c == end || c == start)
          print("*")
        else if (path.contains(c))
          print("O")
        else if (closed.contains(c))
          print("x")
        else if (c.reachable)
          print(".")
        else
          print("#")
      }
      println()
    }
  }
}

val maze = Maze(50, 50, 1352, 1, 1, 31, 39)
//println(maze.solve(1, 1, 31, 39))
maze.solveAstar()
maze.show()


