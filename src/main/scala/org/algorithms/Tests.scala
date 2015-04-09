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
    var eps = 0.02607f // melhores valores para o complex9
    var minPts = 3

    var wines = CSVParser.readFile("datasets/complex9.txt", ',', 0, 1)

    var normalizedWines = Normalization.featureScaling(wines)

    var clusters = DBSCAN(normalizedWines, eps, minPts)

    println("File created with success")
    println("Number of clusters: " + clusters.length.toString)

    // Print all clusters
    for(i <- 0 until clusters.length) {
      println("Cluster " + i + " - Length = " + clusters(i).length.toString)
      for (tupleindex <- clusters(i)) {
        print(tupleindex + " ")
      }
      println("\n")
    }
  }

  def testOPTICS: Unit = {
    var eps = 0.027f
    var minPts = 3
    var maxDistance = eps + 0.01f

    var datasetPoints:Array[OPTICSPoint] = null;

    var wines = CSVParser.readFile("datasets/complex9.txt", ',', 0, 1)
    println("Dataset read.")

    var normalizedWines = Normalization.featureScaling(wines)
    println("Dataset normalized.")

    datasetPoints = new Array[OPTICSPoint](normalizedWines.length)

    println("Populating array of Points.")
    for(iterator <- 0 until datasetPoints.length) {
      datasetPoints(iterator) = new OPTICSPoint(iterator, -1f, -1f, normalizedWines(iterator), false, -1)
    }

    println("Call OPTICS")
    var clusters = OPTICS(datasetPoints, eps, minPts)

    println("Extracting clusters.")
    var writer2 = new PrintWriter(new File("reachDistanceResult.txt"))

    for(i <- 0 until clusters.length) {
      if(clusters(i).reachDistance == -1f) {
        clusters(i).reachDistance = maxDistance
      } else if (clusters(i).coreDistance == -1f) {
        clusters(i).coreDistance = maxDistance
      }

      if(clusters(i).reachDistance == -1f) {
        writer2.write(i + " " + maxDistance + "\n")
      } else {
        writer2.write(i + " " + clusters(i).reachDistance + "\n")
      }
    }

    writer2.close()

    OPTICS.extractDBSCAN(clusters, 0.02607f, 3)

    var writer = new PrintWriter(new File("resultOptics.txt"))

    for(point <- clusters) {
      writer.write(point.dataPoint(0) + " " + point.dataPoint(1) + " " + point.clusterID + "\n")
    }

    writer.close()
  }

  def main(args: Array[String]) {
    testKMeans
    testDBSCAN
    testOPTICS
  }

}
