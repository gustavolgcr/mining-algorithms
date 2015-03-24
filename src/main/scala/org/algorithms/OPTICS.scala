package main.scala.org.algorithms

import scala.collection.mutable.{ArrayBuffer, ListBuffer, PriorityQueue}

object OPTICS {

  var pointState:Array[PointState.PointState] = null;
  var pointInACluster:Array[Boolean] = null;
  var clusters: ListBuffer[ArrayBuffer[Int]] = null;
  var reachabilityDistance:Array[Double] = null;

  object PointState extends Enumeration {
    type PointState = Value
    val Visited, Unvisited, Noise = Value
  }

  def apply(dataset: List[Array[Float]], eps: Float, minPts: Int):ListBuffer[ArrayBuffer[Int]] = {

    clusters = new ListBuffer[ArrayBuffer[Int]]()
    reachabilityDistance = Array.fill(dataset.length)(-1)

    for(it <- 0 until dataset.length) {

      var pointIndex = it
      var neighborPts = regionQuery(dataset, pointIndex, eps)


      pointState(it) = PointState.Visited

      if(coreDistance(dataset, neighborPts, pointIndex, eps, minPts) != -1) {



      }


    }

    return clusters
  }



  def update(dataset: List[Array[Float]], neighbors: Set[Int], pointIndex: Int, Seeds: PriorityQueue[Int], eps: Float, minPoints: Int) : Unit = {

    var coredist = coreDistance(dataset, neighbors, pointIndex, eps, minPoints)

    var neighborList = neighbors.toList

    for (it <- 0 until neighborList.length) {
      if(pointState(it) == PointState.Visited) {

        var newReachDist = math.max(coredist, Similarity.euclidean(dataset(pointIndex), dataset(neighborList(it))))

        if(reachabilityDistance(it) == -1) {
          reachabilityDistance(it) = newReachDist
          Seeds.enqueue()
        }

      }
    }

  }



  //Double check this method
  def coreDistance(dataset: List[Array[Float]], neighbors: Set[Int], pointIndex: Int, eps: Float, minPoints: Int) : Float = {

    var distance = ListBuffer[Float]()

    var neighborList = neighbors.toList
    if(neighborList.length < minPoints) {
      return -1
    } else {

      distance += Similarity.euclidean(dataset(pointIndex), dataset(neighborList(0)))

      for(it <- 1 until neighborList.length) {

        var it2 = distance.length

        while(Similarity.euclidean(dataset(pointIndex), dataset(neighborList(it))) < distance(it2)) {
          distance(it2+1) = distance(it2)
          distance(it2) = Similarity.euclidean(dataset(pointIndex), dataset(neighborList(it)))
          it2 = it2-1
        }
      }
      return distance(minPoints)
    }
  }

  def regionQuery(dataset: List[Array[Float]], pointIndex: Int, eps: Float):
  Set[Int] = {
    var region = ListBuffer[Int]()
    region += pointIndex

    for(it <- 0 until dataset.length) {
      if(Similarity.euclidean(dataset(pointIndex), dataset(it)) <= eps) {
        region += it
      }
    }

    return region.toSet
  }

}
