package vishnu.advent_of_code
import Day5Helper.*

object Day5 {
  def main(args: Array[String]): Unit = {
//    val input = readInput("Day5.sample").toList
    val input = readInput("Day5.input").toList
    val lines = input.map(Line.parse)

    println("star1")
    star1(lines)
    println()
    println("star2")
    star2(lines)

  }
  def star1(lines: Seq[Line]) = {
    val selectedLines = lines.filter(line => line.p1.x == line.p2.x || line.p1.y == line.p2.y)
    val xMax: Int = selectedLines.map(l => Math.max(l.p1.x, l.p2.x)).max
    val yMax = selectedLines.map(l => Math.max(l.p1.y, l.p2.y)).max
    val arr = Array.ofDim[Int](xMax + 1, yMax + 1)
    selectedLines.foreach {
      case (Line(Point(x1, y1), Point(x2, y2))) =>
        if (x1 == x2) {
          for (y <- y1 to y2)
            arr(x1)(y) += 1
        }
        else {
          for (x <- x1 to x2)
            arr(x)(y1) += 1
        }
    }
    println(arr.iterator.flatten.count(_ > 1))

//    arr
//      .map { row =>
//        row.map(cnt => if (cnt == 0) "." else cnt.toString).mkString(" ")
//      }
//      .foreach(println)
  }

  def star2(lines: Seq[Line]) = {
    val xMax: Int = lines.map(l => Math.max(l.p1.x, l.p2.x)).max
    val yMax = lines.map(l => Math.max(l.p1.y, l.p2.y)).max
    val arr = Array.ofDim[Int](xMax + 1, yMax + 1)
    lines.foreach {
      case (Line(Point(x1, y1), Point(x2, y2))) =>
        if (x1 == x2) {
          for (y <- y1 to y2)
            arr(x1)(y) += 1
        }
        else if (y1 == y2) {
          for (x <- x1 to x2)
            arr(x)(y1) += 1
        }
        else {
          var x = x1
          var y = y1
          val yInc = if (y1 > y2) -1 else 1
          while (x <= x2) {
            arr(x)(y) += 1
            x += 1
            y += yInc
          }
        }
    }

//    arr
//      .map { row =>
//        row.map(cnt => if (cnt == 0) "." else cnt.toString).mkString(" ")
//      }
//      .foreach(println)
    println(arr.iterator.flatten.count(_ > 1))
  }

}

object Day5Helper {
  final case class Point(x: Int, y: Int) {
    override def toString: String = s"($x,$y)"

  }
  final case class Line(p1: Point, p2: Point) {
    override def toString: String = s"$p1 -> $p2"

  }

  object Line {
    val lineRegex = "(\\d+),(\\d+)\\s*->\\s*(\\d+),(\\d+)".r
    def apply(
        x1: Int,
        y1: Int,
        x2: Int,
        y2: Int,
      ) =
      new Line(Point(x1, y1), Point(x2, y2))

    def parse(str: String): Line =
      val (x1, y1, x2, y2) = str match {
        case lineRegex(s1, t1, s2, t2) =>
          (s1.toInt, t1.toInt, s2.toInt, t2.toInt)
      }

      if (x1 == x2) {
        if (y1 > y2) {
          Line(x2, y2, x1, y1)
        }
        else {
          Line(x1, y1, x2, y2)
        }
      }
      else {
        if (x1 > x2) {
          Line(x2, y2, x1, y1)
        }
        else {
          Line(x1, y1, x2, y2)
        }
      }

  }

}
