package vishnu.advent_of_code

import vishnu.advent_of_code.Day4Helper.BingoBoard

object Day4Helper {
  import scala.language.unsafeNulls

  final case class BingoBoard(board: Array[Array[Int]]) {
    private val numPosMap =
      (for {
        i <- 0 until board.length
        j <- 0 until board(i).length
      } yield board(i)(j) -> (i -> j)).groupBy(_._1).view.mapValues(_.map(_._2)).toMap

    private val rowSum = Array.ofDim[Int](5)
    private val columSum = Array.ofDim[Int](5)

    def mark(int: Int): Boolean = {
      val positions = numPosMap.getOrElse(int, Seq.empty)
      if (positions.nonEmpty) {
        positions.foreach {
          case (i, j) =>
            board(i)(j) = 0
            rowSum(i) += 1
            columSum(j) += 1
        }
      }
      hasWon
    }

    def hasWon = rowSum.contains(5) || columSum.contains(5)
    def remainingSum = board.map(_.sum).sum

  }
  def getNumbers(str: String, delim: Char = ','): Array[Int] = str.split(delim).filterNot(_.isEmpty).map(_.toInt)

  def getBingoBoard(seqs: Seq[String]) = {
    val board = seqs.map(seq => getNumbers(seq, ' ')).toArray
    BingoBoard(board)
  }

}
object Day4 {
  def main(args: Array[String]): Unit = {

    def getInputs() = {
//      val input = readInput("Day4.sample").toList
      val input = readInput("Day4.input").toList
      val randomNumbers = Day4Helper.getNumbers(input.head).toSeq
      val iterator = input.tail.iterator
      val boards = scala.collection.mutable.ArrayBuffer[BingoBoard]()
      while (iterator.nonEmpty) {
        iterator.next()
        val board = Day4Helper.getBingoBoard(iterator.take(5).toList)
        boards += board
      }
      randomNumbers -> boards.toList
    }

    println("star1")
    val (rnd1, board1) = getInputs()
    star1(rnd1, board1)
    println()
    println("star2")
    val (rnd2, board2) = getInputs()
    star2(rnd2, board2)

  }
  def star1(randomNumber: Seq[Int], bingoBoards: List[BingoBoard]) = {
    // Find first board to win
    var endGame = false
    val iter = randomNumber.iterator
    var lastNum = 0
    while (!endGame & iter.hasNext) {
      lastNum = iter.next()
      val wonBoard = bingoBoards.find(_.mark(lastNum))
      endGame = wonBoard.isDefined
    }
    if (endGame) {
      println(bingoBoards.find(_.hasWon).get.remainingSum)
      println(lastNum)
      println(bingoBoards.find(_.hasWon).get.remainingSum * lastNum)
    }
    else {
      sys.error("No board won")
    }

  }

  def star2(randomNumber: Seq[Int], bingoBoards: List[BingoBoard]) = {
    // find last board to win
    var endGame = false
    val iter = randomNumber.iterator
    var lastNum = 0
    val lst = scala.collection.mutable.Set[BingoBoard](bingoBoards*)
    while (!endGame) {
      val maybeWonBoard = lst.find(_.hasWon)
      maybeWonBoard match {
        case Some(board) =>
          if (lst.size == 1) {
            endGame = true
          }
          else {
            lst.remove(board)
          }

        case None =>
          lastNum = iter.next()
          lst.foreach(_.mark(lastNum))
      }
    }
    val finalBoard = lst.toList.head
    println(finalBoard.remainingSum)
    println(lastNum)
    println(finalBoard.remainingSum * lastNum)
  }

}
