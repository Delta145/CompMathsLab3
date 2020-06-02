package method

object TangentMethod {
  def apply(a: Double, b: Double, e: Double, f: Function[Double, Double], dfdx: Function[Double, Double], d2fdx2: Function[Double, Double]): TangentMethod = {
    new TangentMethod(a, b, e, f, dfdx, d2fdx2)
  }
}

class TangentMethod(a: Double, b: Double, e: Double, f: Function[Double, Double], dfdx: Function[Double, Double], d2fdx2: Function[Double, Double]) {

  def calcWithMethod(): Double = {
    if (f(a)*f(b) >= 0)
      throw new IllegalArgumentException("Не выполняется достаточное условие сходимости: функция имеет одинаковый знак на концах отрезка")
    if (dfdx(a) * dfdx(b) <= 0)
      throw new IllegalArgumentException("Не выполняется достаточное условие сходимости: первая производная не сохраняет знак на концах отрезка")
    if (d2fdx2(a) * d2fdx2(b) <= 0)
      throw new IllegalArgumentException("Не выполняется достаточное условие сходимости: вторая производная не сохраняет знак на концах отрезка")
    else {
      var xi = (b+a)/2.0
      var n = 0;
      while (Math.abs(f(xi)) >= e) {
        n += 1
        xi = xi - f(xi)/dfdx(xi)
        if (n > 1_0_000_000) {
          throw new Exception("Невозможно найти корень. Возможно интервал слишком большой и скорость сходимости крайне низка")
        }
      }
      xi
    }
  }
}
