package main.scala.org.algorithms

/**
 * Created by gustavolgcr on 3/17/15.
 */
object test {
  def main(args: Array[String]) {
    var wines = CSVParser.readFile("datasets/winequality-red.csv")

    var normalizedWines = Normalization.featureScaling(wines, 0, 100)

    for(tuple <- normalizedWines) {
      for(value <- tuple) {
        printf("%.2f\t", value)
      }
      println("")
    }
  }
}
