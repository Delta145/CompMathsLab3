package chart

import java.awt.Color

import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.XYSplineRenderer
import org.jfree.chart.ui.{ApplicationFrame, RectangleInsets}
import org.jfree.chart.{ChartFactory, ChartPanel, JFreeChart}
import org.jfree.data.xy.{XYDataset, XYSeries, XYSeriesCollection}

class EquationChart(title: String, a: Double, b: Double, f: Function[Double, Double], params: List[Double]) extends ApplicationFrame("Lab3") {

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

    chart.setBackgroundPaint(Color.white)

    val plot = chart.getXYPlot
    plot.setBackgroundPaint(new Color(159, 190, 237))
    plot.setDomainGridlinePaint(Color.gray) //сетка
    plot.setRangeGridlinePaint(Color.gray)

    // Определение отступа меток делений
    plot.setAxisOffset(new RectangleInsets(1.0, 1.0, 1.0, 1.0));

    // Скрытие осевых линий и меток делений
    val axis = plot.getDomainAxis()
    axis.setAxisLineVisible(true)   // осевая линия

    val rangeAxis =  plot.getRangeAxis()
    rangeAxis.setAxisLineVisible(true)

    val r = new XYSplineRenderer() //характеристики графика
    r.setPrecision(8)
    r.setSeriesShapesVisible(0, false)
    r.setSeriesPaint(0, Color.blue)

    val dataset = createDataset(a, b, params)
    val xAxis = createXAxis(a, b)

    val r1 = new XYSplineRenderer()

    r.setSeriesPaint(1, Color.black)
    r1.setSeriesShapesVisible(0, false)

    plot.setDataset(0, dataset)
    plot.setDataset(1, xAxis)

    // Подключение Spline Renderer к набору данных
    plot.setRenderer(0, r)
    plot.setRenderer(1, r1)

    chart
  }

  def createDataset(a: Double, b: Double, p: List[Double]): XYDataset = {
    val step = Math.abs(b - a) / 100
    val pa = p(0); val pb = p(1); val pc = p(2); val pd = p(3)
    val dataset = new XYSeries(new String(s"$pa*x^3 + $pb*x^2 + $pc*x + $pd"))
    var i = a
    while (i < b) {
      dataset.add(i, f(i))
      i += step
    }

    val series = new XYSeriesCollection()
    series.addSeries(dataset)

    series
  }

  def createXAxis(a: Double, b: Double): XYDataset = {
    val dataset = new XYSeries(new String("y = 0"))
    dataset.add(a, 0)
    dataset.add(b, 0)

    val series = new XYSeriesCollection()
    series.addSeries(dataset)

    series
  }
}
