package vishnu.advent_of_code

object Day12 {
  import Helper.*
  def main(args: Array[String]): Unit = {
//    def input = createAdjList(readInput("Day12.sample1"))
//    def input = createAdjList(readInput("Day12.sample2"))
//    def input = createAdjList(readInput("Day12.sample3"))
    def input = createAdjList(readInput("Day12.input"))

    println("star1")
    star1(input)
    println()
    println("star2")
    star2(input)

  }
  def star1(adjList: Map[String, Set[String]]) = {
    val paths = scala.collection.mutable.ListBuffer[List[String]]()
    val q = scala.collection.mutable.Queue[(String, Set[String], List[String])]()
    q += (("start", Set.empty, Nil))
    while (q.nonEmpty) {
      val (node, visitedSet, path) = q.dequeue()
      if (node == "end") {
        paths += (node :: path).reverse
      }
      else if (!visitedSet.contains(node)) {
        val neighbors = adjList(node)
        val newVisitedSet = if (node.forall(_.isLower)) {
          visitedSet + node
        }
        else {
          val smallNeighbors = neighbors.filter(neighbor => neighbor.forall(_.isLower))
          val allSmallNeignVisited = smallNeighbors.forall(visitedSet.contains)
          if (allSmallNeignVisited || smallNeighbors.isEmpty) {
            visitedSet + node
          }
          else {
            visitedSet
          }

        }

        neighbors.foreach { neighbor =>
          q += ((neighbor, newVisitedSet, node :: path))
        }
      }
    }

    paths.map(_.mkString("->")).foreach(println)
    println(s"${paths.size}")

  }

  def star2(adjList: Map[String, Set[String]]) = {
    val paths = scala.collection.mutable.ListBuffer[List[String]]()
    val q = scala.collection.mutable.Queue[(String, Set[String], Boolean, List[String])]()
    q += (("start", Set.empty, false, Nil))
    while (q.nonEmpty) {
      val (node, visitedSet, secondVisit, path) = q.dequeue()
      if (node == "end") {
        paths += (node :: path).reverse
      }
      else {
        val isSmallNode = node.forall(_.isLower) && node != "start"
        if (!visitedSet.contains(node) || (isSmallNode && !secondVisit)) {
          val newSecondVisit = if (isSmallNode && visitedSet.contains(node) && !secondVisit) {
            true
          }
          else {
            secondVisit
          }
          val neighbors = adjList(node)
          val newVisitedSet = if (node.forall(_.isLower)) {
            visitedSet + node
          }
          else {
            val smallNeighbors = neighbors.filter(neighbor => neighbor.forall(_.isLower))
            val allSmallNeignVisited = smallNeighbors.forall(visitedSet.contains) & newSecondVisit
            if (allSmallNeignVisited || smallNeighbors.isEmpty) {
              visitedSet + node
            }
            else {
              visitedSet
            }

          }

          neighbors.foreach { neighbor =>
            q += ((neighbor, newVisitedSet, newSecondVisit, node :: path))
          }

        }
      }

    }

    paths.map(_.mkString("->")).foreach(println)
    println(s"${paths.size}")

  }

  object Helper {
    val nodeRegex = "(\\w+)-(\\w+)".r
    def createAdjList(input: Iterator[String]): Map[String, Set[String]] = {
      val adjList = scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]()
      input.foreach {
        case nodeRegex(from, to) =>
          val set1 = adjList.getOrElseUpdate(from, scala.collection.mutable.Set[String]())
          set1.add(to)
          val set2 = adjList.getOrElseUpdate(to, scala.collection.mutable.Set[String]())
          set2.add(from)
      }

      adjList.view.mapValues(_.toSet).toMap
    }

  }

}
