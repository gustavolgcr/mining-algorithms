package main.scala.org.algorithms

import java.io.{File, PrintWriter}


import scala.collection.mutable.{PriorityQueue, HashMap}

object Tests {

  def testKMeans(eps: Float, minPts: Int, datasetName: String): Unit = {

    var dataset = CSVParser.readFile(datasetName)

    var normalizedDataset = Normalization.featureScaling(dataset)

    var clusters = KMeans(normalizedDataset, minPts, eps)

    CSVParser.saveResult("result_kmeans.csv", datasetName, clusters)
  }

  def testDBSCAN(eps: Float, minPts: Int, datasetName: String): Unit = {

    var dataset = CSVParser.readFile(datasetName)

    var normalizedDataset = Normalization.featureScaling(dataset)

    var clusters = DBSCAN(normalizedDataset, eps, minPts)

    CSVParser.saveResult("result_dbscan.csv", datasetName, clusters)
  }

  def testOPTICS(eps: Float, minPts: Int, datasetName: String): Unit = {
    var maxDistance = eps + 0.01f

    var datasetPoints:Array[OPTICSPoint] = null;

    var wines = CSVParser.readFile(datasetName, ';', 1, 4)
    println("Dataset read.")

    var normalizedWines = Normalization.stringDatasetToFloat(wines)

    //var normalizedWines = Normalization.featureScaling(wines)
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

    OPTICS.extractDBSCAN(clusters, 0.11f, 3)

    var writer = new PrintWriter(new File("resultOptics.txt"))

    for(point <- clusters) {
      writer.write(point.dataPoint(0) + " " + point.dataPoint(1) + " " + point.clusterID + "\n")
    }

    writer.close()

    var resultCluster = OPTICS.extractResultClusters(clusters)
    CSVParser.saveResult("result_optics.csv", datasetName, resultCluster)
  }


  val usage = """
    Usage: [-algoritmo kmeans|dbscan|optics -dataset dataset -minPts minPts -eps eps]
              """
  //-algoritmo dbscan -dataset datasets/winequality-red.csv -eps 0.001f -minPts 3

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
          nextOption(map ++ Map('minPts -> value.toInt), tail)
        case "-eps" :: value :: tail =>
          nextOption(map ++ Map('eps -> value.toFloat), tail)
        case string :: opt2 :: tail if isSwitch(opt2) =>
          nextOption(map ++ Map('infile -> string), list.tail)
        case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)

      }
    }
    val options = nextOption(Map(),arglist)

    var minPts = options('minPts).asInstanceOf[Int]
    var eps = options('eps).asInstanceOf[Float]
    var dataset = options('dataset).toString


    options('algoritmo) match {

      case "dbscan"  =>
          testDBSCAN(eps, minPts, dataset)
      case "optics"  =>
          testOPTICS(eps, minPts, dataset)
      case "kmeans"  =>
         testKMeans(eps, minPts, dataset)
      case _  => "Invalid algorithm"
    }


  }

}
