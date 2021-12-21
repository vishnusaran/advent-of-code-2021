package vishnu.advent_of_code

object Day6 {
  def main(args: Array[String]): Unit = {
//    val input = readInput("Day6.sample")
    val input = readInput("Day6.input").toList

    val ages = input.flatMap { line =>
      import scala.language.unsafeNulls
      line.split(",").filterNot(_.isEmpty).map(_.toInt)
    }.toList

    println("star1")
    star1(ages)
    println()
    println("star2")
    star2()

  }
  def star1(ages: List[Int]) = {
    var agesArr = Array.ofDim[Long](9)
    ages.foreach(age => agesArr(age) += 1)

    for (day <- 1 to 256) {
      val zero = agesArr(0)
      for (i <- 1 to 8)
        agesArr(i - 1) = agesArr(i)
      agesArr(6) += zero
      agesArr(8) = zero
      println(s"$day: ${agesArr.sum}")
    }
  }

  def star2() = {}

}
