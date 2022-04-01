object Spiral extends App {

  val n = 5
  var spiral = Array.ofDim[Int](n, n)

  // PATTERN:
  // move n steps right, turn
  // (2x move n-1 steps, turn) until n is 0



  def move(arr: Array[Array[Int]], c: Int, position: (Int, Int), steps: Int, dir: Char) = {

    var (i, j) = position
    var spiral = arr

    dir.toUpper match {
      // Right
      case 'R' => for (x <- 0 until steps) {
        spiral(i + x)(j) = c + x
      }
        i += steps
      // Down
      case 'D' => for (x <- 0 until steps) {
        spiral(i)(j + x) = c + x
      }
        j -= steps
      // Left
      case 'L' => for (x <- 0 until steps) {
        spiral(i - x)(j) = c + x
      }
        i -= steps
      // Up
      case 'U' => for (x <- 0 until steps) {
        spiral(i)(j - x) = c + x
      }
        j += steps

    }
    (spiral, (i, j))
  }

  def turn(dir: Char): Char = {
    val directions = Seq('R', 'D', 'L', 'U')
    val oldIndex = directions.indexOf(dir)
    directions((oldIndex+1) % 4)
  }

  var direction = 'R'
  var position = (0,0)
  var counter = 1

  spiral = move(spiral, counter, (0, 0), n, 'R')._1
  position = (n-1, 0)
  counter += n


  for (x <- n-1 to 0 by -1) {
    for (y <- 0 until 2) {
      direction = turn(direction)

      spiral = move(spiral, counter, position, x, direction)._1
      position = move(spiral, counter, position, x, direction)._2

      counter += n-1
    }
  }

}
