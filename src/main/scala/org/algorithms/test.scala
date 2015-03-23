package main.scala.org.algorithms

import scala.collection.mutable.HashMap

/**
 * Created by gustavolgcr on 3/17/15.
 */
object test {
  def main(args: Array[String]) {

    val wines = CSVParser.readFile("datasets/iris.csv")

    val normalizedWines = Normalization.featureScaling(wines, 0, 1)

    val kmeans : HashMap[Int, Int] = KMeans(normalizedWines, 3, 0.001f)

    CSVParser.saveResult("result.csv", "datasets/iris.csv", kmeans)

  }
}
