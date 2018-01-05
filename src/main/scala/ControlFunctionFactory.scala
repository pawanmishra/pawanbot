
/**
  * Factory class invoked by Scalatron server
  */
class ControlFunctionFactory {
  import ControlFunction._

  def create: String => String = process
}
