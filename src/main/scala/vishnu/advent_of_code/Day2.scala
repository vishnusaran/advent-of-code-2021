package vishnu.advent_of_code
import CommandObject.*

object CommandObject {
  private val inputRegex = "(up|down|forward)\\s*(\\d+)".r
  sealed trait Direction

  case object Forward extends Direction

  case object Up extends Direction

  case object Down extends Direction

  case class Command(direction: Direction, units: Int)

  def parseCommands(inputs: List[String]) =
    inputs.map {
      case inputRegex(directionString, units) =>
        val direction = directionString match {
          case "forward" => Forward
          case "up" => Up
          case "down" => Down
        }
        Command(direction, units.toInt)
    }

}

object Day2 {
  def main(args: Array[String]): Unit = {
    val input = readInput("day2.input").toList
//    val input = readInput("day2.sample").toList
    val commands: List[Command] = parseCommands(input)
    println("star1")
    star1(commands)
    println("star2")
    star2(commands)
  }

  def star1(commands: List[Command]) = {
    var x = 0
    var y = 0
    commands.foreach {
      case Command(command, units) =>
        command match {
          case Forward => x += units
          case Up => y -= units
          case Down => y += units
        }
    }

    println(x * y)
  }

  def star2(commands: List[Command]) = {
    var horizontal = 0
    var depth = 0
    var aim = 0
    commands.foreach {
      case Command(command, units) =>
        command match {
          case Down => aim += units
          case Up => aim -= units
          case Forward =>
            horizontal += units
            depth += (aim * units)
        }
    }
    println(horizontal * depth)
  }

}
