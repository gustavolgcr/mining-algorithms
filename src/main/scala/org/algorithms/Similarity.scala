package main.scala.org.algorithms

/**
 * Created by on 23/03/15.
 */
object Similarity {
  def euclidean(pointA: Array[Float], pointB: Array[Float]) : Float = {
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
}
