package main.scala.org.algorithms

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer, PriorityQueue, HashMap}

object test {
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

//  def HashMapOrdering = new Ordering[HashMap[Int,Float]] {
//    def compare(a : HashMap[Int,Float], b : HashMap[Int,Float]) = b.valuesIterator.next().compare(a.valuesIterator.next())
//
//  }

  def main(args: Array[String]) {
    testKMeans
    testDBSCAN

//    var seeds = PriorityQueue[HashMap[Int, Float]]()(HashMapOrdering)
//
//    seeds.enqueue(HashMap(4 -> 4f), HashMap(234 -> 5f), HashMap(78 -> 6f), HashMap(89 -> 1f))
//
//
//    seeds.find(x => x.get(89) == 1f) match {
//      case Some(hashMap: HashMap[Int, Float]) => hashMap.put(77,32f)//; hashMap.put(234, 234f)
//      case None => println("Not found")
    }

//    pq.find(x => x.priority == 0) match {
//      case Some(elem: Elem) => elem.priority = 3
//      case None => println("Not found")
//    }


//    for(i <- 0 until seeds.length) {
//      println(seeds.dequeue())
//    }




  }
}
