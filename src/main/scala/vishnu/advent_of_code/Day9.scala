package vishnu.advent_of_code

object Day9 {
  import Day9Helper.*
  def main(args: Array[String]): Unit = {
//    val input = toArray(readInput("Day9.sample"))
    val input = toArray(readInput("Day9.input"))

    println("star1")
    star1(input)
    println()
    println("star2")
    star2(input)

  }
  def star1(input: Array[Array[Int]]): List[(Int, Int)] = {
    val lowPoints = scala.collection.mutable.ArrayBuffer[(Int, (Int, Int))]()
    for (i <- input.indices)
      for (j <- input(i).indices) {
        val min = input(i)(j)
        var isMin = true
        if (i > 0 && input(i - 1)(j) <= min) { // top
          isMin = false
        }
        if (i < input.length - 1 && input(i + 1)(j) <= min) { // bottom
          isMin = false
        }
        if (j > 0 && input(i)(j - 1) <= min) { // left
          isMin = false
        }
        if (j < input(i).length - 1 && input(i)(j + 1) <= min) { // right
          isMin = false
        }
        if (isMin) {
          lowPoints += (min -> (i -> j))
        }
      }
    lowPoints.foreach(println)
    println((lowPoints.map(_._1).sum + lowPoints.size))
    lowPoints.map(_._2).toList
  }

  def star2(input: Array[Array[Int]]) = {
    val visited = scala.collection.mutable.Set[(Int, Int)]()
    var basins = scala.collection.mutable.ArrayBuffer[Int]()
    val lowPoints = star1(input)
    println("star2**")
    lowPoints.foreach {
      case point =>
        val q = scala.collection.mutable.Queue[(Int, Int)]()
        q += point
        var score = 0
        while (q.nonEmpty) {
          val (x, y) = q.dequeue()
          if (!visited.contains((x, y))) {
            visited += (x -> y)
            if (input(x)(y) < 9) {
              score += 1

              if (x > 0) q += (x - 1) -> y
              if (x < input.length - 1) q += (x + 1) -> y
              if (y > 0) q += x -> (y - 1)
              if (y < input(x).length - 1) q += x -> (y + 1)
            }
          }
        }
        if (score > 0) {
          basins += score
        }
    }
    basins.foreach(println)
    println(basins.sorted(Ordering.Int.reverse).take(3).foldLeft(1)(_ * _))

  }

}

object Day9Helper {
  def toArray(input: Iterator[String]) =
    input.map(_.iterator).map(str => str.map(c => c - '0').toArray).toArray

}
