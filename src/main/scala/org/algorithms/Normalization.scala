package main.scala.org.algorithms

import scala.collection.mutable.ListBuffer

object Normalization {
  def featureScaling(sourceList: List[Array[String]], a: Float = 0, b:Float = 1) : List[Array[Float]] = {

    var minValues = Array.fill(sourceList(0).length)(0.0)
    var maxValues = Array.fill(sourceList(0).length)(0.0)
    var resultList = ListBuffer[Array[Float]]()
    var tuple = 0
    var it = 0

    // Initializing minValues
    for(it <- 0 until sourceList(0).length) {
      minValues(it) = sourceList(0)(it).toFloat
    }

    // Initializing maxValues
    for(it <- 0 until sourceList(0).length) {
      maxValues(it) = sourceList(0)(it).toFloat
    }

    // Find the max value
    for(tuple <- sourceList) {
      it = 0
      for(it <- 0 until tuple.length) {
        if(tuple(it).toFloat > maxValues(it)) {
          maxValues(it) = tuple(it).toFloat
        }

        if(tuple(it).toFloat < minValues(it)) {
          minValues(it) = tuple(it).toFloat
        }
      }
    }

    // Apply the "Feature Normalization"
    for(tuple <- sourceList) {
      it = 0
      var newTuple = Array.fill(tuple.length)(0f)
      for(it <- 0 until tuple.length) {
        newTuple(it) = a + (((tuple(it).toFloat - minValues(it)) * (b - a)) / (maxValues(it) - minValues(it))).toFloat
      }
      resultList += newTuple
    }

    return resultList.toList
  }

  def stringDatasetToFloat(sourceList: List[Array[String]]) : List[Array[Float]] = {
    var resultList = ListBuffer[Array[Float]]()

    for(tuple <- sourceList) {
      var it = 0
      var newTuple = Array.fill(tuple.length)(0f)
      for(it <- 0 until tuple.length) {
        newTuple(it) = tuple(it).toFloat
      }
      resultList += newTuple
    }

    return resultList.toList
  }
}
