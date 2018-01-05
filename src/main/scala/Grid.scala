import scala.annotation.tailrec
import scala.util.Random

class Grid(input: String) {
  import Grid._

  lazy val open = positions('_')
  lazy val zugars = positions('P')
  lazy val fluppets = positions('B')
  lazy val toxiferas = positions('p')
  lazy val snorgs = positions('b')
  lazy val walls = positions('W')
  lazy val occluded = positions('?')
  lazy val enemyMasters = positions('m')
  lazy val slaveMinis = positions('S')
  lazy val enemyMinis = positions('s')

  def nearByOpenPositions(point: Point, index: Int): Seq[Point] = {
    val x0 = point.x
    val y0 = point.y
    val points = Seq(
      Point(x0 - index, y0),
      Point(x0 - index, y0 - index),
      Point(x0, y0 - index),
      Point(x0 + index, y0 - index),
      Point(x0 + index, y0),
      Point(x0, y0 + index),
      Point(x0 - index, y0 + index),
      Point(x0 + index, y0 + index))
    points
  }

  private def manhattanDistance(p1: Point, p2: Point): Int = {
    Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)
  }

  def nearByFluppet(self: Point): Option[Point] = {
    if (fluppets.isEmpty) {
      None
    } else {
      val result = fluppets.map(x => {
        val distance = manhattanDistance(x, self)
        (distance, x)
      }).minBy(_._1)
      Some(result._2)
    }
  }

  def nearByZugars(self: Point): Option[Point] = {
    if (zugars.isEmpty) {
      None
    } else {
      val result = zugars.map(x => {
        val distance = manhattanDistance(x, self)
        (distance, x)
      }).minBy(_._1)
      Some(result._2)
    }
  }

  private def openPosition(positions: Seq[Point]): Point = {
    @tailrec
    def find(point: Point): Point = {
      open.find(p => point.x == p.x && point.y == p.y) match {
        case Some(x) => x
        case None => find(positions(Random.nextInt(positions.length)))
      }
    }
    find(positions(Random.nextInt(positions.length)))
  }

  private def positions(entity: Char): Set[Point] = {
    val points = for {
      i <- grid.indices
      j <- grid(i).indices
      if grid(i)(j) == entity
    } yield Point(i, j)
    if (points.nonEmpty) points.toSet else Set()
  }

  def move: Seq[Point] = {
    val self = Point(grid.length / 2, grid.length / 2)
    val zugar = nearByZugars(self)
    val fluppets = nearByFluppet(self)

    val target = if (zugar.isDefined) {
      zugar.get
    } else if (fluppets.isDefined) {
      fluppets.get
    } else {
      Point.random
    }
    val path = self.path(target)
    path
  }
}

object Grid {
  var grid: Array[Array[Char]] = Array()

  def apply(input: String): Grid = {
    val dimension = if (input.length == (31 * 31)) 31 else 21
    grid = Array.ofDim[Char](dimension, dimension)
    input.grouped(dimension).zipWithIndex.foreach(x => {
      x._1.toCharArray.zipWithIndex.foreach(v => {
        grid(x._2)(v._2) = v._1
      })
    })
    grid.foreach(x=> println(x.mkString(" ")))
    new Grid(input)
  }
}

