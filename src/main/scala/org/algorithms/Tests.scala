package main.scala.org.algorithms

import scala.collection.mutable
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

    var eps = 0.4f
    var minPts = 4

    var wines = CSVParser.readFile("datasets/winequality-red.csv")

    var normalizedWines = Normalization.featureScaling(wines)

    var clusters = OPTICS(normalizedWines, eps, minPts)

    for(i <- 0 until clusters.length) {

      println("pointIndex: " + clusters(i).pointIndex + " reachDistance: " + clusters(i).reachDistance + " coreDistance: " + clusters(i).coreDistance)
//      println(clusters(i).reachDistance)

    }

//    CSVParser.writeClustersFile("winequality-red-clustered.csv", wines, clusters)
//
//    println("File created with success")
//    println("Numbe of clusters: " + clusters.length.toString)
//
//    // Print all clusters
//    for(i <- 0 until clusters.length) {
//      println("Cluster " + i)
//      for (tupleindex <- clusters(i)) {
//        print(tupleindex + " ")
//      }
//      println("\n")
//    }

  }


  def main(args: Array[String]) {
    //testKMeans
    //testDBSCAN

//    var p = new OPTICSPriorityQueue
//
//    p.seeds.enqueue(new OPTICSPoint(2, 0.0045f))
//    p.seeds.enqueue(new OPTICSPoint(3, 0.01f))
//    p.seeds.enqueue(new OPTICSPoint(4, 46))
//    p.seeds.enqueue(new OPTICSPoint(5, 11))
//    p.seeds.enqueue(new OPTICSPoint(7, 98))
//
//    p.updatePoint(2, 0.005f)
//
//    for(i <- 0 until p.seeds.length) {
//      println(p.seeds.dequeue().reachDistance)
//    }


    testOPTICS

  }
}