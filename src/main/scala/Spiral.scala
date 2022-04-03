class Spiral(n: Int) {

  var spiral = Array.ofDim[Int](n, n)
  
  def turn(dir: Char): Char = {
    val directions = Seq('R', 'D', 'L', 'U')
    val oldIndex = directions.indexOf(dir)
    directions((oldIndex + 1) % 4)
  }

  def changePosition(pos: (Int, Int), dir: Char) = {
    var newDir = dir
    val (y, x) = pos
    var newPos = pos

    // new direction - change direction if it is on the spiral's border
    dir match {
      case 'R' => if (x + 1 == n) newDir = turn(dir)
      else if (spiral(x + 1)(y) != 0) newDir = turn(dir)

      case 'D' => if (y + 1 == n) newDir = turn(dir)
      else if (spiral(y + 1)(x) != 0) newDir = turn(dir)

      case 'L' => if (x-1 < 0) newDir = turn(dir)
      else if (spiral(y)(x-1) != 0) newDir = turn(dir)

      case 'U' => if (spiral(y-1)(x) != 0) newDir = turn(dir)
    }

    // new position
    newDir match {
      case 'R' => newPos = (y, x+1)
      case 'D' => newPos = (y+1, x)
      case 'L' => newPos = (y, x-1)
      case 'U' => newPos = (y-1, x)
    }

    (newPos, newDir)
  }


  def move(pos: (Int, Int), dir: Char, counter: Int) = {
    spiral(pos._1)(pos._2) = counter
  }


  def makeSpiral = {
    var position = (0, 0)
    var direction = 'R'
    var counter = 1

    for (x <- 0 until n * n) {
      move(position, direction, counter)

      val (myPos, myDir) = changePosition(position, direction)
      position = myPos
      direction = myDir
      counter += 1
    }

    spiral
  }
}