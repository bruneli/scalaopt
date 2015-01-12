/*
 * Copyright 2014 Renaud Bruneliere
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scalaopt.stdapps.stats.example

import org.jfree.chart.plot.DatasetRenderingOrder
import org.jfree.chart.renderer.xy.StandardXYItemRenderer
import org.jfree.data.statistics.HistogramDataset
import org.scalaopt.algos._
import org.scalaopt.algos.SeqDataSetConverter._
import org.scalaopt.stdapps.stats._
import scalax.chart.api._

import scala.util.Random

/**
 * Example showing how to fit a sample of events composed from a signal on top of background noise.
 *
 * @author bruneli
 */
object ExSignalPlusBackgroundFit extends App {

  val nbEvents = 2000              // Number of random variates
  val fSignal = 0.3                // Fraction of signal
  val muSignal = 125.0             // Mean of the signal distribution
  val sigmaSignal = 10.0           // Standard deviation of the signal distribution
  val lambdaBkg = 1.0 / 20.0       // Rate of the background distribution
  val (xMin, xMax) = (50.0, 200.0) // (lower, upper) histogram bounds

  val random = new Random(12345)

  /** Generate random variates */
  val data = for (i <- 1 to nbEvents) yield {
    if (random.nextDouble() < fSignal) {
      random.nextGaussian() * sigmaSignal + muSignal
    } else {
      xMin - Math.log(random.nextDouble()) / lambdaBkg
    }
  }

  /** Fit the data with a maximum likelihood method */
  val p0 = Vector(0.05, 125.0, 10.0, 1.0 / 15.0)
  val fit = maxLikelihoodFit(pdf, data map (Vector(_)), p0)

  /** Build a plot with an histogram of the raw data overlaid by the fitted function */
  val histogram = new HistogramDataset
  histogram.addSeries("h1", data toArray, 150, xMin, xMax)
  val xySeries = for (iPt <- 0 to 1000) yield {
    val x = xMin + (xMax - xMin) * iPt / 1000.0
    (x, (fit predict x) * nbEvents.toDouble)
  }
  val chart = XYBarChart(histogram)
  val funcRenderer = new StandardXYItemRenderer(1)
  chart.plot.setDataset(1, xySeries toXYSeriesCollection())
  chart.plot.setRenderer(1, funcRenderer)
  chart.plot.setDatasetRenderingOrder(DatasetRenderingOrder.FORWARD);
  chart.show()

  /**
   * The objective function is the joint pdf of signal plus background with a normalization factor
   */
  def pdf(pars: Coordinates, x: Seq[Double]) =
    if (pars.size != 4) {
      throw new IllegalArgumentException(
        s"pars=($pars) should be a vector of size 5 representing (norm,fSig,mu,sigma,lambda)")
    } else {
      val fSig = pars(0)
      val signalPars = pars.drop(1).take(2)
      val bkgPars = pars.takeRight(1)
      fSig * pdfSignal(signalPars, x) + (1.0 - fSig) * pdfBkg(bkgPars, x)
    }

  /**
   * Signal pdf is a Normal distribution
   */
  def pdfSignal(pars: Coordinates, x: Seq[Double]) =
    if (x.size != 1) {
      throw new IllegalArgumentException(s"x=($x) should be a vector of size 1")
    } else if (pars.size != 2) {
      throw new IllegalArgumentException(s"pars=($pars) should be a vector of size 2 representing (mu, sigma)")
    } else if (pars(1) == 0.0) {
      throw new IllegalArgumentException(s"sigma=${pars(1)} should be != 0")
    } else {
      val z = (x(0) - pars(0)) / pars(1)
      Math.exp(-z * z / 2.0) / Math.sqrt(2.0 * Math.PI) / pars(1)
    }

  /**
   * Background pdf is an Exponential distribution
   */
  def pdfBkg(pars: Coordinates, x: Seq[Double]) =
    if (x.size != 1) {
      throw new IllegalArgumentException(s"x=($x) should be a vector of size 1")
    } else if (pars.size != 1) {
      throw new IllegalArgumentException(s"pars=($pars) should be a vector of size 1 representing lambda")
    } else {
      pars(0) * Math.exp(-pars(0) * (x(0) - xMin))
    }

}
