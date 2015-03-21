package main.scala.org.algorithms

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Created by rui on 3/19/15.
 */
object DBSCAN {
  var pointState:Array[PointState.PointState] = null;
  var clusters: ListBuffer[ArrayBuffer[Int]] = null;

  object PointState extends Enumeration {
    type PointState = Value
    val Visited, Unvisited, Noise = Value
  }

  def euclideanDistance(pointA: Array[Float], pointB: Array[Float]) : Float = {
    var distance : Float = 0

    if (pointA.length == pointB.length) {
      for (i <- 0 until pointA.length) {
        distance += scala.math.pow(pointA(i) - pointB(i), 2).toFloat
      }

      distance = scala.math.sqrt(distance).toFloat
      return distance
    }

    return -1
  }

  def apply(dataset: List[Array[Float]], eps: Float, minPts: Int):
    ListBuffer[ArrayBuffer[Int]] = {
    clusters = new ListBuffer[ArrayBuffer[Int]]()
    pointState = Array.fill(dataset.length)(PointState.Unvisited)

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

    return clusters
  }

  def expandCluster(dataset: List[Array[Float]], pointIndex: Int, neighborPts:
    Set[Int], eps: Float, minPts: Int) = {
    var neighborPtsCpy = neighborPts.toSet
    var cluster = new ArrayBuffer[Int]()

    cluster += pointIndex
    clusters += cluster

    for(pointIndexInNeighborPts <- neighborPtsCpy) {
      if(pointState(pointIndexInNeighborPts) == PointState.Unvisited) {
        pointState(pointIndexInNeighborPts) = PointState.Visited
        var neighborPts_ = regionQuery(dataset, pointIndexInNeighborPts, eps)
        if(neighborPts_.size >= minPts) {
          neighborPtsCpy = neighborPtsCpy.union(neighborPts_)
        }
      }

      for(clusterInClusters <- clusters) {
        if(!clusterInClusters.contains(pointIndexInNeighborPts)) {
          cluster += pointIndexInNeighborPts
        }
      }
    }
  }

  def regionQuery(dataset: List[Array[Float]], pointIndex: Int, eps: Float):
    Set[Int] = {
    var region = ListBuffer[Int]()
    region += pointIndex

    for(it <- 0 until dataset.length) {
      if(euclideanDistance(dataset(pointIndex), dataset(it)) <= eps) {
        region += it
      }
    }

    return region.toSet
  }
}
