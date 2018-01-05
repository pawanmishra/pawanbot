
object Model {

  case class Command(command: String, parameters: Map[String, String]) {
    override def toString: String = {
      val params = parameters.map(x => s"${x._1}=${x._2}").mkString(",")
      s"${command}($params)"
    }
  }
}