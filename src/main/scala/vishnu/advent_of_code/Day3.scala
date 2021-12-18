package vishnu.advent_of_code
import scala.collection.mutable

object Day3 {
  def main(args: Array[String]): Unit = {
//    val input = readInput("Day3.sample").toList
    val input = readInput("Day3.input").toList

    println("star1")
    star1(input)
    println()
    println("star2")
    star2(input)

  }
  def star1(inputs: List[String]) = {
    val countArr: Array[Int] = getCounts(inputs)
    val gamma = countArr.map(count => if (count > (inputs.size - count)) 1 else 0).mkString
    val epsilon = gamma.map(char => if (char == '1') '0' else '1')
    println(s"${toNumber(gamma)}")
    println(s"${toNumber(epsilon)}")
    println(toNumber(gamma) * toNumber(epsilon))
  }

  def star2(inputs: List[String]) = {
    val o2Rating = getLastStanding(inputs, (count, len) => if (count >= (len - count)) '1' else '0')
    println(s"O2Rtaing:$o2Rating ::: ${toNumber(o2Rating)}")
    val cO2Rating = getLastStanding(inputs, (count, len) => if ((len - count) <= count) '0' else '1')
    println(s"CO2Rtaing:$cO2Rating ::: ${toNumber(cO2Rating)}")
    println(toNumber(o2Rating) * toNumber(cO2Rating))
  }

  def toNumber(str: String) = {
    var value = 0
    str.zipWithIndex.foreach {
      case (char, pos) =>
        char match {
          case '1' => value |= (1 << (str.length - 1 - pos))
          case _ =>
        }
    }
    value
  }

  private def getLastStanding(inputs: List[String], commonCharGen: (Int, Int) => Char) = {
    val q: mutable.Queue[String] = scala.collection.mutable.Queue[String](inputs*)
    for (pos <- 0 until inputs.head.length) {
      val count = getCountAt(q, pos)
      val mostCommon = commonCharGen(count, q.size)
      if (q.size > 1) {
        for (_ <- 0 until q.size) {
          val str = q.dequeue()
          if (str(pos) == mostCommon) {
            q.enqueue(str)
          }
        }
//        println(s"${pos + 1}:($mostCommon)")
//        q.foreach(println)
      }
    }
    require(q.size == 1)
    q.dequeue()
  }

  private def getCounts(inputs: Iterable[String]) = {
    val countArr = Array.ofDim[Int](inputs.head.length)
    inputs.foreach { binary =>
      binary.zipWithIndex.foreach {
        case (char, pos) =>
          char match {
            case '1' => countArr(pos) += 1
            case _ =>
          }
      }
    }
    countArr
  }

  private def getCountAt(inputs: Iterable[String], pos: Int) = {
    var count = 0
    inputs.foreach { input =>
      input(pos) match {
        case '1' => count += 1
        case _ =>
      }
    }
    count
  }

}
