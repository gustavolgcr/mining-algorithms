package main.scala.org.algorithms

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Created by gustavolgcr on 3/23/15.
 */
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



      if(pointState(it) == PointState.Unvisited) {

        pointState(it) = PointState.Visited
        var neighborPts = regionQuery(dataset, pointIndex, eps)
      }

    }

    return clusters
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
