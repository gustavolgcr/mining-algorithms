package main.scala.org.algorithms

import scala.collection.mutable.ListBuffer

/**
 * Created by rui on 3/18/15.
 */
object Normalization {
  def featureScaling(sourceList: List[Array[String]], a: Float = 0, b:Float = 1) : ListBuffer[Array[Double]] = {
    var minValues = Array.fill(sourceList(0).length)(0.0)
    var maxValues = Array.fill(sourceList(0).length)(0.0)
    var resultList = ListBuffer[Array[Double]]()
    var tuple = 0
    var it = 0

    // Initializing minValues
    for(it <- 0 until sourceList(0).length) {
      minValues(it) = sourceList(0)(it).toDouble
    }

    // Initializing maxValues
    for(it <- 0 until sourceList(0).length) {
      maxValues(it) = sourceList(0)(it).toDouble
    }

    // Find the max value
    for(tuple <- sourceList) {
      it = 0
      for(it <- 0 until tuple.length) {
        if(tuple(it).toDouble > maxValues(it)) {
          maxValues(it) = tuple(it).toDouble
        }

        if(tuple(it).toDouble < minValues(it)) {
          minValues(it) = tuple(it).toDouble
        }
      }
    }

    // Apply the "Feature Normalization"
    for(tuple <- sourceList) {
      it = 0
      var newTuple = Array.fill(tuple.length)(0.0)
      for(it <- 0 until tuple.length) {
        newTuple(it) = a + ((tuple(it).toDouble - minValues(it)) * (b - a)) / (maxValues(it) - minValues(it))
      }
      resultList += newTuple
    }

    return resultList
  }
}
