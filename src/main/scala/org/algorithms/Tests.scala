package main.scala.org.algorithms

import java.io.{File, PrintWriter}


import scala.collection.mutable.{PriorityQueue, HashMap}

object Tests {

  def testKMeans(eps: Float, minPts: Int, dataset: String): Unit = {

    val wines = CSVParser.readFile(dataset)

    val normalizedWines = Normalization.featureScaling(wines, 0, 1)

    val kmeans : HashMap[Int, Int] = KMeans(normalizedWines, minPts, eps)

    CSVParser.saveResult("result.csv", dataset, kmeans)
  }

  def testDBSCAN(eps: Float, minPts: Int, dataset: String): Unit = {

    var wines = CSVParser.readFile(dataset, ',', 0, 1)

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

  def testOPTICS(eps: Float, minPts: Int, dataset: String): Unit = {
    var maxDistance = eps + 0.01f

    var datasetPoints:Array[OPTICSPoint] = null;

    var wines = CSVParser.readFile(dataset, ',', 0, 1)
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

    if (args.length == 0) println(usage)
    val arglist = args.toList
    type OptionMap = Map[Symbol, Any]
    def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
      def isSwitch(s : String) = (s(0) == '-')
      list match {
        case Nil => map
        case "-algoritmo" :: value :: tail =>
          nextOption(map ++ Map('algoritmo -> value), tail)
        case "-dataset" :: value :: tail =>
          nextOption(map ++ Map('dataset -> value), tail)
        case "-minPts" :: value :: tail =>
          nextOption(map ++ Map('minPts -> value), tail)
        case "-eps" :: value :: tail =>
          nextOption(map ++ Map('eps -> value), tail)
        case string :: opt2 :: tail if isSwitch(opt2) =>
          nextOption(map ++ Map('infile -> string), list.tail)
        case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)
        case option :: tail => println("Unknown option "+option)
          exit(1)
      }
    }
    val options = nextOption(Map(),arglist)

    var minPts = options('minPts).toInt
    var eps = options('eps).toFloat
    var dataset = options('dataset).toString


    options('algoritmo) match {

      case "dbscan"  => println("dbscan")
        testDBSCAN(eps, minPts, dataset)
      case "optics"  => println("optics")
        testOPTICS(eps, minPts, dataset)
      case "kmeans"  => println("kmeans")
        testKMeans(eps, minPts, dataset)
      case _  => "Invalid month"
    }



  }

}
