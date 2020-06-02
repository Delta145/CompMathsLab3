package io

import chart.{EquationChart, SystemEquationChart}
import method.{ChordMethod, NewtonSystemMethod, TangentMethod}

import scala.io.StdIn

class CommandHandler {
  def handleUserInput(): Unit = {
    var funN: Option[Int] = None
    while (funN.isEmpty || funN.get < 1 || funN.get > 3 ) {
      println("Что мы хотим решать?\n" +
        "1) нелинейное уравнение a*x^3 - b*x^2 - c*x + d методом хорд\n" +
        "2) нелинейное уравнение a*x^3 - b*x^2 - c*x + d методом секущих\n" +
        "3) систему нелинейных уравнений \n" +
        "\ty = a*sin(x) + b\n" +
        "\ty = c*e^x + d")
      try {
        funN = Option(StdIn.readInt())
      } catch {
        case x: Exception => println("Введите 1, 2, или 3")
      }
    }

    var default = true
    println("Хотите ли вы сами ввести параметры a,b,c,d? Если нет - будут значения по-умолчанию")
    if (funN.get != 3)
      println("нелинейное уравение по-умолчанию x^3 - 3.125*x^2 - 3.5*x + 2.458")
    else
      println("система нелинейных уравнений по-умолчанию:\n" +
        "\ty = sin(x) - 3.125\n" +
        "\ty = -3.5*e^x + 2.458")
    if (StdIn.readLine().toLowerCase().equals("да"))
      default = false

    var a: Option[Double] = None
    var b: Option[Double] = None
    var c: Option[Double] = None
    var d: Option[Double] = None

    if (!default) {
      while (a.isEmpty) {
        println("Введите a")
        a = readDoubleSafe()
      }
      while (b.isEmpty) {
        println("Введите b")
        b = readDoubleSafe()
      }
      while (c.isEmpty) {
        println("Введите c")
        c = readDoubleSafe()
      }
      while (d.isEmpty) {
        println("Введите d")
        d = readDoubleSafe()
      }
    } else {
      a = Some(1.0)
      b = Some(-3.125)
      c = Some(-3.5)
      d = Some(2.458)
    }


    var high: Option[Double] = None
    var low: Option[Double] = None
    if (funN.get != 3) {
      while (low.isEmpty) {
        println("Введите левую границу отрезка")
        low = readDoubleSafe()
      }
      while (high.isEmpty || high.get <= low.get) {
        println("Введите правую границу отрезка. Она должна быть больше левой границы!")
        high = readDoubleSafe()
      }
    } else {
      println("Хотите ли вы сами задать приближенное значение для системы? Если нет - программа сама их выберет")
      if (StdIn.readLine().toLowerCase().equals("да")) {
        while (low.isEmpty) {
          println("Введите приближенное значение \'x\'")
          low = readDoubleSafe()
        }
        while (high.isEmpty) {
          println("Введите приближенное значение \'y\'")
          high = readDoubleSafe()
        }
      }
    }

    var accuracy: Option[Double] = None
    while (accuracy.isEmpty || accuracy.get <= 0) {
      println("Введите точность. Точность должна быть положительным числом")
      accuracy = readDoubleSafe()
    }

    val functions =
      List[Double => Double](x => a.get*Math.pow(x, 3) + b.get*Math.pow(x, 2) + c.get*x + d.get)
    val dfdx =
      List[Double => Double](x => a.get*3*Math.pow(x, 2) + b.get*2*x + c.get)
    val d2fdx2 =
      List[Double => Double](x => a.get*6*x + b.get*2)

    val systemOf2Functions =
      List[(Double, Double) => Double](
        (x, y) => a.get*Math.sin(x) + b.get - y,
        (x, y) => c.get*Math.pow(Math.E, x) - y + d.get)

    val systemOf2NormalForm =
      List[(Double) => Double](
        (x) => a.get*Math.sin(x) + b.get,
        (x) => c.get*Math.pow(Math.E, x) + d.get)

    val systemOf2dfdx =
      List[(Double, Double) => Double](
        (x, y) => a.get*Math.cos(x),
        (x, y) => c.get*Math.pow(Math.E, x))

    val systemOf2dfdy =
      List[(Double, Double) => Double](
        (x, y) => -1,
        (x, y) => -1)

    val systemOf2FM =
      List[Double](if (low.isEmpty) { b.get+Math.E/5 } else low.get+Math.E/5, if (high.isEmpty) { 0+Math.E/5 + d.get } else high.get+Math.E/5)

    val params =
      List[Double](a.get, b.get, c.get, d.get)

    try {
      funN.get match {

        case 1 => {
          val result = ChordMethod(low.get, high.get, accuracy.get, functions.head, dfdx.head, d2fdx2.head).calcWithMethod()
          println(s"Ответ: $result (метод хорд)")
          new EquationChart("Метод хорд", low.get, high.get, functions.head, params).drawChart()
        }

        case 2 => {
          val result = TangentMethod(low.get, high.get, accuracy.get, functions.head, dfdx.head, d2fdx2.head).calcWithMethod()
          println(s"Ответ: $result (метод касательных)")
          new EquationChart("Метод касательных", low.get, high.get, functions.head, params).drawChart()
        }

        case 3 => {
          val resultSystem = new NewtonSystemMethod(systemOf2Functions, systemOf2dfdx, systemOf2dfdy, systemOf2FM, accuracy.get).solveSystem()
          val x = resultSystem(0)
          val y = resultSystem(1)
          println(s"Решение системы: x=$x, y=$y")
          new SystemEquationChart("Метод ньютона для системы уравнений", x, y,
            systemOf2NormalForm(0), systemOf2NormalForm(1), params).drawChart()
        }
      }
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
