package vishnu.advent_of_code

object Day7 {
  def main(args: Array[String]): Unit = {
//    val input = readInput("Day7.sample").toList
    val input = readInput("Day7.input").toList

    val positions = input.flatMap { line =>
      import scala.language.unsafeNulls
      line.split(",").filterNot(_.isEmpty).map(_.toInt)
    }.toArray

    println(positions.max)

    println("star1")
    star1(positions)
    println()
    println("star2")
    star2(positions)

  }
  def star1(positions: Array[Int]) = {
    val posArrLen = positions.max + 1
    var prevArr = Array.ofDim[Int](posArrLen)
    for (j <- positions.indices.reverse) {
      val arr = Array.ofDim[Int](posArrLen)
      val myPos = positions(j)
      for (i <- prevArr.indices)
        arr(i) = Math.abs(myPos - i) + prevArr(i)
      prevArr = arr
    }
    println(prevArr.min)
  }

  def star2(positions: Array[Int]) = {
    val posArrLen = positions.max + 1
    var prevArr = Array.ofDim[Int](posArrLen)
    for (j <- positions.indices.reverse) {
      val arr = Array.ofDim[Int](posArrLen)
      val myPos = positions(j)
      for (i <- prevArr.indices)
        val n = Math.abs(myPos - i)
        val cost = n * (n + 1) / 2
        arr(i) = cost + prevArr(i)
      prevArr = arr
    }
    println(prevArr.min)
  }

}
