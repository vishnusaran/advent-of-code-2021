package vishnu.advent_of_code

object Day11 {
  import Helper.*
  def main(args: Array[String]): Unit = {
//    def input = toInput(readInput("Day11.simple_sample"))
//    def input = toInput(readInput("Day11.sample"))
    def input = toInput(readInput("Day11.input"))

    println("star1")
    star1(input)
    println()
    println("star2")
    star2(input)

  }
  def star1(octos: Array[Array[Int]]) = {
    var totalFlashes = 0L
    for (i <- 0 until 100) {
      val flashed = scala.collection.mutable.Set[(Int, Int)]()
      val q = scala.collection.mutable.Queue[(Int, Int)]()

      def incAndAddToQueue(x: Int, y: Int) =
        if (x >= 0 && x < octos.length && y >= 0 && y < octos(0).length) {
          octos(x)(y) += 1
          if (octos(x)(y) > 9) {
            q += (x -> y)
          }
        }

      for (x <- octos.indices)
        for (y <- octos(x).indices) {
          octos(x)(y) += 1
          if (octos(x)(y) > 9) {
            q.enqueue(x -> y)
          }
        }

      while (q.nonEmpty) {
        val point @ (x, y) = q.dequeue()
        if (!flashed.contains(point)) {
          flashed += point
          incAndAddToQueue(x - 1, y - 1)
          incAndAddToQueue(x - 1, y)
          incAndAddToQueue(x - 1, y + 1)
          incAndAddToQueue(x, y - 1)
          incAndAddToQueue(x, y + 1)
          incAndAddToQueue(x + 1, y - 1)
          incAndAddToQueue(x + 1, y)
          incAndAddToQueue(x + 1, y + 1)
        }
      }
      flashed.foreach {
        case (x, y) =>
          octos(x)(y) = 0
      }
      totalFlashes += flashed.size
//      if ((i + 1) % 10 == 0) {
//        println(s"After step ${i + 1}:")
//        print(octos)
//      }

    }
    println(s"Total Flashes:${totalFlashes}")
  }

  def star2(octos: Array[Array[Int]]) = {
    var syncFlashed = false
    var i = 1
    while (!syncFlashed) {
      val flashed = scala.collection.mutable.Set[(Int, Int)]()
      val q = scala.collection.mutable.Queue[(Int, Int)]()

      def incAndAddToQueue(x: Int, y: Int) =
        if (x >= 0 && x < octos.length && y >= 0 && y < octos(0).length) {
          octos(x)(y) += 1
          if (octos(x)(y) > 9) {
            q += (x -> y)
          }
        }

      for (x <- octos.indices)
        for (y <- octos(x).indices) {
          octos(x)(y) += 1
          if (octos(x)(y) > 9) {
            q.enqueue(x -> y)
          }
        }

      while (q.nonEmpty) {
        val point @ (x, y) = q.dequeue()
        if (!flashed.contains(point)) {
          flashed += point
          incAndAddToQueue(x - 1, y - 1)
          incAndAddToQueue(x - 1, y)
          incAndAddToQueue(x - 1, y + 1)
          incAndAddToQueue(x, y - 1)
          incAndAddToQueue(x, y + 1)
          incAndAddToQueue(x + 1, y - 1)
          incAndAddToQueue(x + 1, y)
          incAndAddToQueue(x + 1, y + 1)
        }
      }
      flashed.foreach {
        case (x, y) =>
          octos(x)(y) = 0
      }
      if (flashed.size == octos.length * octos(0).length) {
        syncFlashed = true
        println(s"Sync Flashed at ${i}")
      }
//      if (i >= 193 && i <= 195) {
//        println(s"After step ${i}:")
//        print(octos)
//      }
      i += 1
    }
  }

  object Helper {
    def toInput(iter: Iterator[String]): Array[Array[Int]] =
      iter.map(_.toArray.map(a => a - '0')).toArray

    def print(input: Array[Array[Int]]) = {
      input.map(_.mkString).foreach(println)
      println
    }

  }

}
