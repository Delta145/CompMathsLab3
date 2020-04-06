package io

import method.{ChordMethod, TangentMethod}

import scala.io.StdIn

class CommandHandler {
  def handleUserInput(): Unit = {
    var funN: Option[Int] = None
//    while (funN.isEmpty || funN.get < 1 || funN.get > 5 ) {
//      println("Выберите интеграл из списка:\n" +
//        "1) sin(x)\n" +
//        "2) cos(x)\n" +
//        "3) 5\n" +
//        "4) x^3/3\n" +
//        "5) a*x^3 - b*x^2 - c*x + d")
//      try {
//        funN = Option(StdIn.readInt())
//      } catch {
//        case x: Exception => println("Введите 1, 2, 3, 4 или 5")
//      }
//    }

    var low: Option[Double] = None
    while (low.isEmpty) {
      println("Введите левую границу отрезка")
      low = readDoubleSafe()
    }

    var high: Option[Double] = None
    while (high.isEmpty || high.get <= low.get ) {
      println("Введите правую границу отрезка. Она должна быть больше левой границы!")
      high = readDoubleSafe()
    }

    var accuracy: Option[Double] = None
    while (accuracy.isEmpty || accuracy.get <= 0) {
      println("Введите точность. Точность должна быть положительным числом")
      accuracy = readDoubleSafe()
    }
    "x^3 - 3.125*x^2 - 3.5*x + 2.458"
    val functions =
      List[Double => Double](x => Math.pow(x, 3) - 3.125*Math.pow(x, 2) - 3.5*x + 2.458)
    val deriatives =
      List[Double => Double](x => 3*Math.pow(x, 2) - 6.25*x - 3.5)
    val secondDeriatives =
      List[Double => Double](x => 6*x - 6.25)
    try {
      val result = ChordMethod(low.get, high.get, accuracy.get, functions.head, deriatives.head).calcWithMethod()
      val resultTangent = TangentMethod(low.get, high.get, accuracy.get, functions.head, deriatives.head).calcWithMethod()
      println(s"Ответ: $result (метод хорд), $resultTangent (метод касательных)")
    } catch {
      case e: Exception => println(e.getMessage)
    }
  }

  def readDoubleSafe(): Option[Double] = {
    try {
      Option(StdIn.readDouble())
    } catch {
      case x: Exception => println("введите вещественное число")
        None
    }
  }
}
