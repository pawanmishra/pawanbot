import Model.Command

/**
  * Sample TypeClasses implementation
  * @tparam A
  */
trait Dump[A] {
  def dump(a: A): String
}

object Dump {

  def apply[A](implicit sh: Dump[A]): Dump[A] = sh

  def dump[A](a: A)(implicit d: Dump[A]) = d.dump(a)

  def instance[A](func: A => String): Dump[A] =
    new Dump[A] {
      override def dump(a: A): String = func(a)
    }

  implicit class DumpOps[A: Dump](a: A) {
    def dump = Dump[A].dump(a)
  }

  implicit val pointDump: Dump[Point] =
    instance(point => s"${point.x}:${point.y}")

  implicit val commandDump: Dump[Command] =
    instance(command => {
      val params = command.parameters.map(x => s"${x._1}=${x._2}").mkString(",")
      s"${command.command}($params)"
    })

}
