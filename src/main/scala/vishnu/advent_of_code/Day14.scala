package vishnu.advent_of_code

object Day14 extends TimeOps {
  import Helper.*
  def main(args: Array[String]): Unit = {
//    def input = parseInput(readInput("Day14.sample"))
    def input = parseInput(readInput("Day14.input"))

    println("star1")
    (star1(input)).time
    println()
    println("star2")
    (star2(input)).time

  }
  def star1(input: (String, Map[(Char, Char), Char])) = {
    val countArr = Array.ofDim[Int](26)
    val (template, rules) = input
    template.foreach { char =>
      countArr(char - 'A') += 1
    }

    var str = template

    for (i <- 1 to 10) {
      val stringBuilder = new StringBuilder()
      str.toArray.sliding(2).map(arr => arr(0) -> arr(1)).foreach {
        case tup @ (left, right) =>
          val middle = rules(tup)
          countArr(middle - 'A') += 1
          stringBuilder.append(left)
          stringBuilder.append(middle)
//          stringBuilder.append(right)
      }
      stringBuilder.append(str.last)
      str = stringBuilder.toString()
//      println(s"After step $i: $str")
    }
    println((countArr.max - countArr.filter(_ != 0).min))
  }

  def star2(input: (String, Map[(Char, Char), Char])) = {
    val (template, rules) = input
    val arrPosMap = rules.keySet.zipWithIndex.toMap
    var levelArr = Array.fill[Array[BigInt]](arrPosMap.size)(Array.fill[BigInt](26)(BigInt(0)))
    rules.keySet.foreach {
      case tup @ (left, right) =>
        levelArr(arrPosMap(tup))(left - 'A') += 1
        levelArr(arrPosMap(tup))(right - 'A') += 1
    }

    for (i <- 1 to 40) {
      val nextLevelArr = Array.ofDim[Array[BigInt]](arrPosMap.size)
      rules.keySet.foreach {
        case tup @ (left, right) =>
          val middle = rules(tup)
          val newArr = mergeAlphaBetArr(levelArr(arrPosMap(left -> middle)), levelArr(arrPosMap(middle -> right)))
          newArr(middle - 'A') -= 1
          nextLevelArr(arrPosMap(tup)) = newArr
      }
      levelArr = nextLevelArr
    }

    val finalArr =
      template.toArray.sliding(2).map(arr => arr(0) -> arr(1)).foldLeft(Array.fill[BigInt](26)(BigInt(0))) {
        case (agg, tup) =>
          mergeAlphaBetArr(agg, levelArr(arrPosMap(tup)))
      }
    println((finalArr.max - finalArr.filter(_ != 0).min))

  }

  object Helper {
    val parseRegex = "([A-Z])([A-Z])\\s*->\\s*([A-Z])".r
    def mergeAlphaBetArr(left: Array[BigInt], right: Array[BigInt]): Array[BigInt] =
      (left zip right).map { case (l, r) => l + r }

    def parseInput(iter: Iterator[String]): (String, Map[(Char, Char), Char]) = {
      val template = iter.take(1).toList.head
      val rules = iter
        .filter(_.nonEmpty)
        .map {
          case parseRegex(left1, left2, right) =>
            (left1.head -> left2.head) -> right.head
        }
        .toMap
      template -> rules
    }

  }

}
