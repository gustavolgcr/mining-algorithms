package main.scala.org.algorithms

/**
 * Created by gustavolgcr on 3/17/15.
 */
object test {
  def main(args: Array[String]) {
    var wines = CSVParser.readFile("datasets/winequality-red.csv")

    var normalizedWines = Normalization.featureScaling(wines, 0, 100)

    var kmeans = new KMeans(normalizedWines, 8, .5f)

    var answer : Array[Cluster] = kmeans.execute

    println(answer)

  }
}
