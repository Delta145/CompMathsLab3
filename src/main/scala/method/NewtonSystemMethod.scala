package method

import lab1.GaussMethod

class NewtonSystemMethod(funtions: List[(Double, Double) => Double], dfdxs: List[(Double, Double) => Double], dfdys: List[(Double, Double) => Double], freeMembers: List[Double], E: Double) {
  def solveSystem(): List[Double] = {
    var xi = freeMembers(0)
    var yi = freeMembers(1)
    while (Math.abs(funtions(0)(xi, yi) - funtions(1)(xi, yi)) > E) {

      val firstLine = List[java.lang.Double](dfdxs(0)(xi, yi), dfdys(0)(xi, yi), -funtions(0)(xi, yi))
      val secondLine = List[java.lang.Double](dfdxs(1)(xi, yi), dfdys(1)(xi, yi), -funtions(1)(xi, yi))
      val answer = new GaussMethod(firstLine, secondLine).getAnswer

      val gn = answer(0)
      val hn = answer(1)
      xi = xi + gn
      yi = yi + hn
    }

    List[Double](xi, yi)
  }
}
