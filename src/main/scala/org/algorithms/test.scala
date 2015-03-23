package main.scala.org.algorithms

import scala.collection.mutable.HashMap

/**
 * Created by gustavolgcr on 3/17/15.
 */
object test {
  def main(args: Array[String]) {

<<<<<<< HEAD
    val wines = CSVParser.readFile("datasets/iris.csv")
=======
    var wines = CSVParser.readFile("datasets/winequality-red.csv")

    var normalizedWines = Normalization.featureScaling(wines, 0, 100)

    var answer = KMeans(normalizedWines, 6, 0.1f)
>>>>>>> 2aef93f009d20ca051967bf90f0fdb6ebe2f4f65

    val normalizedWines = Normalization.featureScaling(wines, 0, 1)

    val kmeans : HashMap[Int, Int] = KMeans(normalizedWines, 3, 0.001f)

    CSVParser.saveResult("result.csv", "datasets/iris.csv", kmeans)

  }
}
