package vishnu.advent_of_code
import scala.language.unsafeNulls

object Day8 {
  def main(args: Array[String]): Unit = {
//    val input = readInput("Day8.sample")
    val input = readInput("Day8.input")
    val parts = input.map(_.split("\\|")).map(splits => splits(0).trim -> splits(1).trim).toList
    val formatedInput = parts.map {
      case (left, right) =>
        left.split(" ").toList -> right.split(" ").toList
    }

    println("star1")
    star1(parts.iterator)
    println()
    println("star2")
    star2(formatedInput)

  }
  def star1(inputs: Iterator[(String, String)]) = {
    val counts = inputs
      .map(_._2)
      .iterator
      .flatMap(_.split(" ").iterator)
      .count(str => str.length == 2 || str.length == 4 || str.length == 3 || str.length == 7)
    println(s"${counts}")
  }

  def star2(inputs: Seq[(List[String], List[String])]) = {
    import Day8Helper.*
    println(s"${bitFreq.toList.sortBy(_._1)}")
    println(numIdentifier.toList.sortBy(_._1))
    val outputs = inputs.map {
      case (left, right) =>
        val cntMap = Array.ofDim[Int](7)
        left.foreach { disp =>
          disp.foreach { char =>
            cntMap(char - 'a') += 1
          }
        }
        val dispToNumMap = left.map { disp =>
          val sum = disp.map(char => cntMap(char - 'a')).sum
          val actualNumber = numIdentifier(sum)
          disp.toSet -> actualNumber
        }.toMap

        right.map(str => dispToNumMap(str.toSet)).mkString.toLong
    }
//    outputs.foreach(println)
    println(outputs.sum)

  }

}

object Day8Helper {
  val map = Map[Int, Set[Char]](
    0 -> Set('a', 'b', 'c', 'e', 'f', 'g'),
    1 -> Set('c', 'f'),
    2 -> Set('a', 'c', 'd', 'e', 'g'),
    3 -> Set('a', 'c', 'd', 'f', 'g'),
    4 -> Set('b', 'c', 'd', 'f'),
    5 -> Set('a', 'b', 'd', 'f', 'g'),
    6 -> Set('a', 'b', 'd', 'e', 'f', 'g'),
    7 -> Set('a', 'c', 'f'),
    8 -> Set('a', 'b', 'c', 'd', 'e', 'f', 'g'),
    9 -> Set('a', 'b', 'c', 'd', 'f', 'g'),
  )

  val bitFreq = map.values.flatten.groupBy(identity).view.mapValues(_.toList.size).toMap

  val numIdentifier = map.view.mapValues(set => set.iterator.map(a => bitFreq(a)).sum).map(_.swap).toMap

}
