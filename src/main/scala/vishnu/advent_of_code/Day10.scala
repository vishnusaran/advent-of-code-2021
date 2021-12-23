package vishnu.advent_of_code

object Day10 {
  import Day10Helper.*
  def main(args: Array[String]): Unit = {
//    def input = readInput("Day10.sample")
    def input = readInput("Day10.input")

    println("star1")
    star1(input)
    println()
    println("star2")
    star2(input)

  }
  def star1(inputs: Iterator[String]) =
    val invalidChars = inputs.map { input =>
      val stack = scala.collection.mutable.Stack[Char]()
      var invalidChar: Option[Char] = None
      val charArr = input.toArray
      var i = 0
      while (invalidChar.isEmpty && i < charArr.length) {
        if (openChars.contains(charArr(i))) {
          stack += charArr(i)
        }
        else {

          if (stack.isEmpty || matcingChars(stack.removeLast()) != charArr(i)) {
            invalidChar = Some(charArr(i))
          }
        }
        i += 1
      }
      invalidChar
    }.toList
//    invalidChars.foreach(println)
    println(invalidChars.collect { case Some(x) => sytaxErrorScore(x) }.sum)

  def star2(inputs: Iterator[String]) =
    val autoCompleteScores = inputs.map { input =>
      val stack = scala.collection.mutable.Stack[Char]()
      var invalidCharFound = false
      val charArr = input.toArray
      var i = 0
      while (!invalidCharFound && i < charArr.length) {
        if (openChars.contains(charArr(i))) {
          stack += charArr(i)
        }
        else {
          if (stack.isEmpty || matcingChars(stack.removeLast()) != charArr(i)) {
            invalidCharFound = true
          }
        }
        i += 1
      }
      if (invalidCharFound) {
        0L
      }
      else {
        var score = 0L
        while (stack.nonEmpty) {
          score *= 5
          score += autoCompleteScore(matcingChars(stack.removeLast()))
        }
        score
      }
    }.toList

    val sortedScores = autoCompleteScores.filter(_ > 0).sorted
    println(sortedScores(sortedScores.size / 2))

}
object Day10Helper {
  val sytaxErrorScore = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137,
  )

  val autoCompleteScore = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4,
  )
  val matcingChars = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>',
  )
  val openChars = matcingChars.keySet
  val closeChars = sytaxErrorScore.keySet

}
