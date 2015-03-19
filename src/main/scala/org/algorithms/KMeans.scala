package main.scala.org.algorithms

import scala.util.Random

/**
 * Created by Iago on 19/03/2015.
 */
class KMeans(data: List[Array[Float]], k : Int, cutoff : Float) {

  def execute: Boolean = {

    /* Select k objects for initial centroids */
    var initial : Array[Point] = new Array[Point](k)
    var clusters :  Array[Cluster] = new Array[Cluster](k)
    for(i <- 0 until k) {
      initial(i) = new Point(data(Random.nextInt(data.size)))
      clusters(i) = new Cluster(List(initial(i)))
      clusters(i).calcCentroid()
    }

    var biggest_shift : Float = -1
    /* Iterate all objects */
    while (biggest_shift == -1 || biggest_shift > cutoff) {

      var lists: Array[List[Point]] = new Array[List[Point]](k)

      for (d <- data) {
        var p = new Point(d)
        var smallest_distance = p.distanceBetween(clusters(0).centroid)
        var index = 0

        for (i <- 0 until k) {
          var distance = p.distanceBetween(clusters(i + 1).centroid)
          if (distance < smallest_distance) {
            smallest_distance = distance
            index = i + 1
          }
        }

        lists(index) ::= p
      }

      /* Check the value of modification, if so small then break! */
      biggest_shift = 0
      for (i <- 0 until k) {
        var shift = clusters(i).update(lists(i))
        biggest_shift = math.max(biggest_shift, shift)
      }

    }

    return false
  }
}

/*
* Point Definition
* N attributes
*/
class Point(_attributes: Array[Float]) {
  var attributes : Array[Float] = _attributes

  /* Euclidian distance */
  def distanceBetween(point: Point) : Float = {
    var distance : Float = 0

    if (attributes.length == point.attributes.length) {

      for (i <- 0 until this.attributes.length) {
        distance += (this.attributes(i) - point.attributes(i))*(this.attributes(i) - point.attributes(i))
      }
      distance = scala.math.sqrt(distance).toFloat
      return distance

    }

    return -1
  }
}


class Cluster(_points: List[Point]) {
  var points : List[Point] = _points
  var centroid : Point = new Point(Array(0))

  def update(_points: List[Point]): Float = {
    var old_centroid : Point  = this.centroid
    this.points = _points
    this.centroid = this.calcCentroid()
    return old_centroid.distanceBetween(this.centroid)
  }

  /*
  * Calculate the centroid of cluster
  */
  def calcCentroid() : Point = {
    var centroid : Array[Float] = new Array[Float](this.points.length)
    for (p <- this.points) {
      for (i <- 0 until p.attributes.length) {
        centroid(i) += p.attributes(i)/this.points.length
      }
    }
    return new Point(centroid)
  }

}