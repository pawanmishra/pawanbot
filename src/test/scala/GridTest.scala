import org.scalatest.FlatSpec

class GridTest extends FlatSpec {

  it should "Parse input string" in {
    val input = "____________________________________________________________________________________________?_____________________________WW_____________________________WW_____________________________WW_________b_______p___________WW_______________________P_____WW_____________________________WW_____________________________WW_____________________________WW____________________________BWW_____b_______________________WW_________________b_______P___WW_________________________p___WW_______________M_____________WW?????WWWWWWW_________________WW???????????__________________WW????????_________WWWWWWWWWWWWWW?????____________WWWWWWWWWWWWWW??_______________????????????WW_________________??????????????____B_____________?????????????__________________?????????????___________________????????????___________________????????????___________________????????????W___________________???????????____________________???????????_____________________??????????_____________________??????????"
    val grid = Grid(input)

    //grid.zugars.foreach(println(_))

    val point = Point(15, 15)

    //val close = grid.nearByOpenPositions(point)

    val nearByFluppets = grid.nearByFluppet(point)
    println(nearByFluppets.getOrElse("Fluppets not found"))
    val nearByZugar = grid.nearByZugars(point)
    println(nearByZugar.getOrElse("Zugar not found"))
    val path = grid.move
    path.foreach(println(_))
  }
}
