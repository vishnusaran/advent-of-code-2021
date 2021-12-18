package vishnu.advent_of_code

object Day1 {
  def main(args: Array[String]): Unit = {
    val input = readInput("day1.input").map(_.toInt).toList
    star2(input)
  }

  def star1(input: Seq[Int]) =
    println(input.sliding(2).count(pair => pair(1) > pair.head))

  def star2(input: Seq[Int]) =
    println(input.sliding(3).map(_.sum).sliding(2).count(pair => pair(1) > pair.head))

}
