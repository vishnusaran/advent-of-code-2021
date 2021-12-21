package vishnu

import java.io.InputStream

package object advent_of_code {
  def readInput(fileName: String): Iterator[String] = {
    val inputStreamOrNull = getClass.getResourceAsStream(s"/$fileName")
    inputStreamOrNull match {
      case is: InputStream => scala.io.Source.fromInputStream(is).getLines()
      case _ => sys.error(s"Cannot find file $fileName")
    }
  }

  trait TimeOps {
    import scala.concurrent.duration.*
    import scala.language.postfixOps
    implicit class TimeUtils[A](a: => A) {
      def :@ : A =
        time

      def time: A = {
        val start = System.currentTimeMillis()
        val rel = a
        val timeTaken = (System.currentTimeMillis() - start).millisecond
        println(s"Time taken: ${readableTime(timeTaken.toCoarsest)}")
        rel
      }

      private def readableTime(time: FiniteDuration): String = {
        def loop(
            length: Long,
            unit: TimeUnit,
            parts: List[String],
          ): String = {
          def coarserOrThis(coarser: TimeUnit, divider: Int): String = {
            val q = length / divider
            val rem = length % divider
            val newParts = if (rem != 0) FiniteDuration(rem, unit).toString() :: parts else parts
            if (q != 0) {
              loop(q, coarser, newParts)
            }
            else {
              newParts.mkString(",")
            }
          }
          unit match {
            case DAYS => (FiniteDuration(length, unit).toString() :: parts).mkString(",")
            case HOURS => coarserOrThis(DAYS, 24)
            case MINUTES => coarserOrThis(HOURS, 60)
            case SECONDS => coarserOrThis(MINUTES, 60)
            case MILLISECONDS => coarserOrThis(SECONDS, 1000)
            case MICROSECONDS => coarserOrThis(MILLISECONDS, 1000)
            case NANOSECONDS => coarserOrThis(MICROSECONDS, 1000)
          }
        }

        if (time.unit == DAYS || time.length == 0) this.toString
        else loop(time.length, time.unit, Nil)
      }

    }

  }

}
