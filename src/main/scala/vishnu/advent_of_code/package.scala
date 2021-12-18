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

}
