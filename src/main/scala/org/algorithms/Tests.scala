package main.scala.org.algorithms

import java.io.{File, PrintWriter}


import scala.collection.mutable.{PriorityQueue, HashMap}

object Tests {

  def testKMeans(): Unit = {
    val wines = CSVParser.readFile("datasets/winequality-red.csv")

    val normalizedWines = Normalization.featureScaling(wines, 0, 1)

    val kmeans : HashMap[Int, Int] = KMeans(normalizedWines, 3, 0.001f)

    CSVParser.saveResult("result.csv", "datasets/winequality-red.csv", kmeans)
  }

  def testDBSCAN: Unit = {
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
      for (tupleindex <- clusters(i)) {
        print(tupleindex + " ")
      }
      println("\n")
    }
  }

  def testOPTICS: Unit = {

    var eps = 0.1f
    var minPts = 4

    var datasetPoints:Array[OPTICSPoint] = null;

    var wines = CSVParser.readFile("datasets/diamonds9.txt", ',', 0, 1)
    println("Dataset read.")

    var normalizedWines = Normalization.featureScaling(wines)
    println("Dataset normalized.")

    datasetPoints = new Array[OPTICSPoint](normalizedWines.length)

    println("Populating array of Points.")
    for(iterator <- 0 until datasetPoints.length) {

      datasetPoints(iterator) = new OPTICSPoint(iterator, -1, -1, normalizedWines(iterator), false, -1)
    }

    var clusters = OPTICS(datasetPoints, eps, minPts)

    println("Clusters found.")

    println("Extracting clusters.")
    OPTICS.extractDBSCAN(clusters, 0.035f, 4)

    var writer = new PrintWriter(new File("resultOptics.txt"))

    for(i <- 0 until clusters.length) {

      for(iterator <- clusters(i).dataPoint) {
        writer.write(iterator + " ")
      }
      writer.write(clusters(i).clusterID + "\n")

    }

    writer.close()

  }

  def main(args: Array[String]) {

    testKMeans
    testDBSCAN
    testOPTICS

  }

}
