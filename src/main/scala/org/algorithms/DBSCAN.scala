package main.scala.org.algorithms

import scala.collection.mutable.{ArrayBuffer, ListBuffer, HashMap}

object DBSCAN {
  var pointState:Array[PointState.PointState] = null;
  var pointInACluster:Array[Boolean] = null;
  var resultClusters: HashMap[Int, Int] = new HashMap[Int, Int]();
  var clusterId: Int = -1;

  object PointState extends Enumeration {
    type PointState = Value
    val Visited, Unvisited, Noise = Value
  }

  def apply(dataset: List[Array[Float]], eps: Float, minPts: Int):
  HashMap[Int, Int] = {
    pointState = Array.fill(dataset.length)(PointState.Unvisited)
    pointInACluster = Array.fill(dataset.length)(false)

    for(it <- 0 until dataset.length) {
      var pointIndex = it
      if(pointState(it) == PointState.Unvisited) {
        pointState(it) = PointState.Visited
        var neighborPts = regionQuery(dataset, pointIndex, eps)

        if(neighborPts.size < minPts) {
          pointState(it) = PointState.Noise

        } else {
          expandCluster(dataset, pointIndex, neighborPts, eps, minPts)
        }
      }
    }

    return resultClusters
  }

  def expandCluster(dataset: List[Array[Float]], pointIndex: Int, neighborPts:
    Set[Int], eps: Float, minPts: Int) = {
    var neighborPtsCpy = neighborPts.toList

    clusterId = clusterId + 1
    resultClusters += (pointIndex -> clusterId)
    pointInACluster(pointIndex) = true

    var i = 0
    while(i < neighborPtsCpy.length) {
      var tempLen = neighborPtsCpy.length

      if(pointState(neighborPtsCpy(i)) == PointState.Unvisited) {
        pointState(neighborPtsCpy(i)) = PointState.Visited
        var neighborPts_ = regionQuery(dataset, neighborPtsCpy(i), eps)

        if(neighborPts_.size >= minPts) {
          neighborPtsCpy = neighborPtsCpy ++ neighborPts_.diff(neighborPtsCpy.toSet).toList
        }
      }

      if(!pointInACluster(neighborPtsCpy(i))) {
        for(point <- neighborPtsCpy) resultClusters += (point -> clusterId)
        pointInACluster(neighborPtsCpy(i)) = true
      }

      i = i + 1
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
