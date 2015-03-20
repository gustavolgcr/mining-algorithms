package main.scala.org.algorithms

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Created by rui on 3/19/15.
 */
object DBSCAN {
  object PointState extends Enumeration {
    type PointState = Value
    val Visited, Unvisited, Noise = Value
  }

  def apply(dataset: List[Array[Float]], eps: Float, minPts: Int):
                    ListBuffer[ArrayBuffer[Int]] = {
    var clusters = new ListBuffer[ArrayBuffer[Int]]()
    var pointState = Array.fill(dataset.length)(PointState.Unvisited)

    for(it <- 0 until dataset.length) {
      var point = dataset(it)
      if(pointState(it) == PointState.Unvisited) {
        pointState(it) = PointState.Visited
        var neighborPts = regionQuery(point, eps)
        if(neighborPts.length < minPts) {
          pointState(it) = PointState.Noise
        } else {
          var cluster = new ArrayBuffer[Int]()
          clusters += cluster
          expandCluster(point, neighborPts, cluster, eps, minPts)
        }
      }
    }
    
    return clusters
  }

  def expandCluster(point: Array[Float], neighbotPts: List[Array[Float]],
                    cluster: ArrayBuffer[Int], eps: Float, minPts: Int) = {

  }

  def regionQuery(point: Array[Float], eps: Float): List[Array[Float]] = {

    return null
  }
}
