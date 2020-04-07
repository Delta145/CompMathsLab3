package chart

import java.awt.Color

import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.{XYBarRenderer, XYSplineRenderer}
import org.jfree.chart.ui.{ApplicationFrame, RectangleInsets}
import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart}
import org.jfree.data.xy.{XYDataset, XYSeries, XYSeriesCollection}

class SystemEquationChart(title: String, x: Double, y: Double, first: (Double) => Double, second: (Double) => Double, params: List[Double]) extends ApplicationFrame("Lab3") {

  def drawChart(): Unit = {
    val chart = createChart()
    val panel = new ChartPanel(chart)
    panel.setPreferredSize(new java.awt.Dimension(560, 480))
    setContentPane(panel)
    pack()
    setVisible(true)
  }

  def createChart(): JFreeChart = {
    val chart = ChartFactory.createXYLineChart(
      title,
      "x",
      "y",
      null,
      PlotOrientation.VERTICAL,
      true,
      false,
      false)

    val plot = chart.getXYPlot
    plot.setBackgroundPaint(new Color(159, 190, 237))

    val r = new XYSplineRenderer() //характеристики графика

//    r.setSeriesShapesVisible(0, false)
    r.setSeriesPaint(0, Color.blue)

    val r1 = new XYSplineRenderer()

    r1.setSeriesPaint(0, Color.RED)
//    r1.setSeriesShapesVisible(0, false)

    val a = params(0); val b = params(1); val c = params(2); val d = params(3)

    val dataset1 = createDataset(x-0.5, x+0.5, first, s"y = $a*sin(x) + $b")
    val dataset2 = createDataset(x-0.5, x+0.5, second, s"y = $c*e^x + $d")

    plot.setDataset(0, dataset1)
    plot.setDataset(2, dataset2)

    // Подключение Spline Renderer к набору данных
    plot.setRenderer(0, r)
    plot.setRenderer(2, r1)

    chart
  }

  def createDataset(a: Double, b: Double, f: Double => Double, desc: String): XYDataset = {
    val step = Math.abs(b - a) / 200
    val dataset = new XYSeries(new String(desc))
    var i = a
    while (i < b) {
      dataset.add(i, f(i))
      i += step
    }

    val series = new XYSeriesCollection()
    series.addSeries(dataset)

    series
  }
}
