import scala.util.Random

class Point(val x: Int, val y: Int) {

  def signum = Point(x.signum, y.signum)

  def path(other: Point): Seq[Point] = {
    val quadrant = if (other.x < x && other.y > y) Position.FirstQuadrant
    else if (other.x < x && other.y < y) Position.SecondQuadrant
    else if (other.x > x && other.y < y) Position.ThirdQuadrant
    else if (other.x > x && other.y > y) Position.FourthQuadrant
    else if (other.x == x) Position.XAxis
    else Position.YAxis

    quadrant match {
      case Position.FirstQuadrant|Position.ThirdQuadrant|Position.XAxis|Position.YAxis => {
        val temp = Point(other.y, other.x)
        val diff = Point(temp.x - x, temp.y - y)
        trace(diff)
      }
      case Position.SecondQuadrant|Position.FourthQuadrant => {
        val diff = Point(other.x - x, other.y - y)
        trace(diff)
      }
    }
  }

  private def trace(point: Point): Seq[Point] = {
    val xMove = point.x > 0 match {
      case true => for { _ <- 1 to point.x } yield Direction.Right
      case false => for { _ <- 1 to Math.abs(point.x) } yield Direction.Left
    }

    val yMove = point.y > 0 match {
      case true => for { _ <- 1 to point.y } yield Direction.Down
      case false => for { _ <- 1 to Math.abs(point.y) } yield Direction.Up
    }

    xMove ++ yMove
  }

  override def toString: String = {
    s"$x:$y"
  }
}

object Point {

  private var rnd = new Random()

  def apply(x: Int, y: Int): Point = new Point(x, y)

  def create(input: String): Point = {
    val values = input.split(":").map(_.toInt)
    Point(values(0), values(1))
  }

  def random: Point = {
    val x = rnd.nextInt(3) - 1
    val y = rnd.nextInt(3) - 1
    new Point(x, y)
  }
}

object Direction {
  val Left = Point(-1, 0)
  val Right = Point(1, 0)
  val Up = Point(0, -1)
  val Down = Point(0, 1)
  val UpLeft = Point(-1, -1)
  val DownLeft = Point(-1, 1)
  val UpRight = Point(1, -1)
  val DownRight = Point(1, 1)
}

object Position {
  val FirstQuadrant = 0
  val SecondQuadrant = 1
  val ThirdQuadrant = 3
  val FourthQuadrant = 4
  val XAxis = 5
  val YAxis = 6
}



