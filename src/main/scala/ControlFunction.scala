import scala.util.Random

object ControlFunction {

  import CommandParser._
  import Model._

  private val roles = Seq("fluppetHunter", "zugarHunter", "detonator")

  def process(input: String): String = {
    val command = parse(input)
    command.command match {
      case "React" => {
        command.parameters("generation") match {
          case "0" => processMasterBot(command)
          case _ => processMiniBot(command)
        }
      }
      case _ => ""
    }
  }

  private def processMasterBot(command: Command): String = {

    def newMove(path: Seq[Point]): String = {
      val markedCells = Seq()//path.map(x => Command("MarkCell", Map("position" -> x.toString, "color" -> "#ff8800")))
      val head = path.head
      val tail = path.tail.map(_.toString).mkString("&")

      if (tail.isEmpty) {
        (Seq(Command("Move", Map("direction" -> head.toString)),
          Command("Set", Map("path" -> ""))) ++ markedCells).mkString("|")
      } else {
        (Seq(Command("Move", Map("direction" -> head.toString)),
          Command("Set", Map("path" -> tail))) ++ markedCells).mkString("|")
      }
    }

    val output = if (command.parameters.contains("path")) {
      println(s"Inside auxilary block block")
      val path = command.parameters("path").split("&").map(x => Point.create(x))
      newMove(path)
    } else {
      println(s"Inside main block")
      val grid = Grid(command.parameters("view"))
      val path = grid.move
      newMove(path)
    }
    output
  }

  private def processMiniBot(command: Command): String = {
    val point = Point(0,0)//grid.move
    Command("Move", Map("direction" -> point.toString)).toString
  }
}
