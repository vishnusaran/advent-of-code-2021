package vishnu.advent_of_code

object Day13 {
  import Helper.*
  def main(args: Array[String]): Unit = {
//    def input = parseInput(readInput("Day13.sample"))
    def input = parseInput(readInput("Day13.input"))

    println("star1")
    star1(input)
    println()
    println("star2")
    star2(input)

  }
  def star1(input: (Set[Point], List[FoldAlong])) = {
    val (points, foldAt) = input
    val newPoints = foldAt.take(1).foldLeft(points) {
      case (interPoints, foldAlong) =>
        foldAlong match {
          case FoldLeft(at) => foldLeft(interPoints, at)
          case FoldUp(at) => foldUp(interPoints, at)
        }
    }

    println(newPoints.size)
  }
  def star2(input: (Set[Point], List[FoldAlong])) = {
    val (points, foldAt) = input
    val newPoints = foldAt.foldLeft(points) {
      case (interPoints, foldAlong) =>
        foldAlong match {
          case FoldLeft(at) => foldLeft(interPoints, at)
          case FoldUp(at) => foldUp(interPoints, at)
        }
    }
    printPoints(newPoints)
  }

  private def foldUp(points: Set[Point], at: Int): Set[Point] = {
    val resultPoints = scala.collection.mutable.Set[Point]()
    points.foreach {
      case point @ Point(x, y) =>
        if (x < at) {
          resultPoints += point
        }
        else if (x > at) {
          val relPoint = Point(at - (x - at), y)
          resultPoints += relPoint
        }
    }
    resultPoints.toSet
  }

  private def foldLeft(points: Set[Point], at: Int) = {
    val resultPoints = scala.collection.mutable.Set[Point]()
    points.foreach {
      case point @ Point(x, y) =>
        if (y < at) {
          resultPoints += point
        }
        else if (y > at) {
          val relPoint = Point(x, at - (y - at))
          if (points.contains(relPoint)) {
            println(s"Overlap at ${relPoint}")
          }
          resultPoints += relPoint
        }
    }
    resultPoints.toSet
  }

  object Helper {
    sealed trait FoldAlong

    final case class FoldUp(at: Int) extends FoldAlong

    final case class FoldLeft(at: Int) extends FoldAlong

    def printPoints(points: Set[Point]) = {
      val maxX = points.map(_.x).max
      val maxY = points.map(_.y).max
      for (i <- 0 to maxX)
        for (j <- 0 to maxY)
          if (points.contains(Point(i, j))) {
            print('#')
          }
          else {
            print('.')
          }
        println

    }

    object FoldAlong {
      val foldAlongRegex = "fold along (x|y)=(\\d+)".r
      def apply(str: String): FoldAlong = str match {
        case foldAlongRegex("y", at) => FoldUp(at.toInt)
        case foldAlongRegex("x", at) => FoldLeft(at.toInt)
      }

    }
    final case class Point(x: Int, y: Int)
    val pointRegex = "(\\d+),(\\d+)".r

    def parseInput(iter: Iterator[String]) = {
      val points = iter
        .takeWhile(_.nonEmpty)
        .map {
          case pointRegex(x, y) =>
            Point(y.toInt, x.toInt)
        }
        .toSet

      val foldAlongs = iter.filter(_.nonEmpty).map(FoldAlong.apply).toList
      (points -> foldAlongs)
    }

  }

}
