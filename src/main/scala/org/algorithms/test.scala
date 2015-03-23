package main.scala.org.algorithms

/**
 * Created by gustavolgcr on 3/17/15.
 */
object test {
  def main(args: Array[String]) {

    var eps = 0.1f
    var minPts = 4

    var wines = CSVParser.readFile("datasets/winequality-red.csv")

    var normalizedWines = Normalization.featureScaling(wines)

    var clusters = DBSCAN(normalizedWines, eps, minPts)

    CSVParser.writeClustersFile("winequality-red-clustered.csv", wines, clusters)

    println("File created with success")
    println("Numbe of clusters: " + clusters.length.toString)

    // Print all clusters
    for(i <- 0 until clusters.length) {
      println("Cluster " + i)
      for(tupleindex <- clusters(i)) {
        print(tupleindex + " ")
      }
      println("\n")
    }
  }
}
