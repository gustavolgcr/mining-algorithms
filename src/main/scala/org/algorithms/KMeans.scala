package main.scala.org.algorithms

import scala.util.Random

/**
 * Created by Iago on 19/03/2015.
 */
class KMeans(data: List[Array[Float]], k : Int, cutoff : Float) {

  def execute: Array[Cluster] = {

    /* Select k objects for initial centroids */
    val initial : Array[Point] = new Array[Point](k)
    val clusters :  Array[Cluster] = new Array[Cluster](k)
    for(i <- 0 until k) {
      initial(i) = new Point(data(Random.nextInt(data.size)))
      clusters(i) = new Cluster(List(initial(i)))
      clusters(i).updateCentroid()
    }

    var biggest_shift : Float = -1
    /* Iterate all objects */
    while (biggest_shift == -1 || biggest_shift > cutoff) {

      val lists: Array[List[Point]] = new Array[List[Point]](k)

      /* Initializing the list */
      for (i <- 0 until k) {
        lists(i) = List[Point](null)
      }

      for (d <- data) {
        var p = new Point(d)
        var smallest_distance = p.distanceBetween(clusters(0).centroid)
        var index = 0

        for (i <- 0 until k) {
          val distance = p.distanceBetween(clusters(i).centroid)
          if (distance < smallest_distance) {
            smallest_distance = distance
            index = i
          }
        }

        lists(index) ::= p
      }

      /* Check the value of modification, if so small then break! */
      biggest_shift = 0
      for (i <- 0 until k) {
        val shift = clusters(i).update(lists(i))
        biggest_shift = math.max(biggest_shift, shift)
      }

    }

    clusters
  }
}

/*
* Point Definition
* N attributes
*/
class Point(_attributes: Array[Float]) {
  var attributes : Array[Float] = _attributes

  /* Euclidean distance */
  def distanceBetween(point: Point) : Float = {
    var distance : Float = 0

    if (attributes.length == point.attributes.length) {

      for (i <- 0 until this.attributes.length) {
        distance += (this.attributes(i) - point.attributes(i))*(this.attributes(i) - point.attributes(i))
      }
      distance = scala.math.sqrt(distance).toFloat
      return distance

    }

    -1
  }
}


class Cluster(_points: List[Point]) {
  var points : List[Point] = _points
  var centroid : Point = null

  def update(_points: List[Point]): Float = {
    val old_centroid : Point  = this.centroid
    this.points = _points
    this.centroid = this.calcCentroid()
    old_centroid.distanceBetween(this.centroid)
  }

  /*
  * Calculate the centroid of cluster
  */
  def calcCentroid() : Point = {
    val centroid : Array[Float] = new Array[Float](this.points.head.attributes.length)
    for (p <- this.points) {
      if (p != null) {
        for (i <- 0 until p.attributes.length) {
          centroid(i) += p.attributes(i) / this.points.length
        }
      }
    }
    new Point(centroid)
  }

  def updateCentroid() {
    this.centroid = this.calcCentroid()
  }

}