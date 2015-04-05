package main.scala.org.algorithms

import scala.collection.mutable.{ArrayBuffer, ListBuffer, PriorityQueue, HashMap}

class OPTICSPoint(_pointIndex: Int, _reachDistance: Float = -1, _coreDistance: Float = -1,
                  _dataPoint: Array[Float] = null, _isProcessed: Boolean = false, _clusterId: Int = -1) {

  var pointIndex: Int = _pointIndex
  var reachDistance: Float = Float.MaxValue
  var coreDistance: Float = _coreDistance
  var dataPoint: Array[Float] = _dataPoint
  var isProcessed: Boolean = _isProcessed
  var clusterID: Int = _clusterId

}

class OPTICSPriorityQueue {

  var seeds = PriorityQueue[OPTICSPoint]()(OPTICSPointOrdering)

  def OPTICSPointOrdering = new Ordering[OPTICSPoint] {
    def compare(a : OPTICSPoint, b : OPTICSPoint) = b.reachDistance.compareTo(a.reachDistance)
  }

  def updatePoint(point: Int, reachDistance: Float) = {
    this.seeds.find(x => Some(x.pointIndex) == Some(point)) match {
      case Some(p: OPTICSPoint) => {
        p.reachDistance = reachDistance
        this.seeds = this.seeds.clone()
      }
      case None =>
    }
  }
}

object OPTICS {

  var priorityQueue: OPTICSPriorityQueue = null;

  def apply(datasetPoints: Array[OPTICSPoint], eps: Float, minPts: Int) : ListBuffer[OPTICSPoint] = {

    priorityQueue = new OPTICSPriorityQueue
    var resulList = new ListBuffer[OPTICSPoint]

    for(point <- datasetPoints) {

      if(point.isProcessed == false) {

        var neighborhoodPoints = regionQuery(datasetPoints, point, eps)

        point.isProcessed = true
        point.coreDistance = coreDistance(datasetPoints, neighborhoodPoints, point, eps, minPts)
        resulList += point

        if (point.coreDistance < Float.MaxValue) {

          update(datasetPoints, neighborhoodPoints, point, priorityQueue, eps, minPts)

          while(!priorityQueue.seeds.isEmpty) {

            var pointNeighbor = priorityQueue.seeds.dequeue()

            neighborhoodPoints = regionQuery(datasetPoints, pointNeighbor, eps)
            pointNeighbor.coreDistance = coreDistance(datasetPoints, neighborhoodPoints, pointNeighbor, eps, minPts)
            resulList += pointNeighbor

            if(pointNeighbor.coreDistance != -1) {

              update(datasetPoints, neighborhoodPoints, point, priorityQueue, eps, minPts)
            }
          }
        }
      }
    }

    return resulList
  }

  def update(datasetPoints: Array[OPTICSPoint], neighborhoodPoints: Set[OPTICSPoint], point: OPTICSPoint, priorityQueue: OPTICSPriorityQueue, eps: Float, minPoints: Int) : Unit = {

    var coreDistance = point.coreDistance
    var neighborhoodList = neighborhoodPoints.toList

    for (neighborhoodIterator <- neighborhoodList) {
      if(neighborhoodIterator.isProcessed == false) {

        var newReachDistance = math.max(coreDistance, Similarity.euclidean(neighborhoodIterator.dataPoint, point.dataPoint))

        if(neighborhoodIterator.reachDistance == -1) {

          neighborhoodIterator.reachDistance = newReachDistance

          priorityQueue.seeds.enqueue(neighborhoodIterator)

        } else {

          if (newReachDistance < neighborhoodIterator.reachDistance) {
            neighborhoodIterator.reachDistance = newReachDistance
            priorityQueue.updatePoint(neighborhoodIterator.pointIndex, newReachDistance)

          }
        }
      }
    }
  }

  def coreDistance(datasetPoints: Array[OPTICSPoint], neighborhoodPoints: Set[OPTICSPoint], point: OPTICSPoint, eps: Float, minPoints: Int) : Float = {

    def ordering = new Ordering[Float] {
      def compare(a : Float, b : Float) = b.compareTo(a)
    }

    var distance = new PriorityQueue[Float]()(ordering)

    var neighborhoodList = neighborhoodPoints.toList

    if(neighborhoodList.length < minPoints) {

      return -1

    } else {

      for(it <- 1 until neighborhoodList.length) {
        distance.enqueue(Similarity.euclidean(point.dataPoint, neighborhoodList(it).dataPoint))
      }

      for(it <- 0 until minPoints-2) {
        distance.dequeue()
      }

      return distance.dequeue()
    }
  }

  def regionQuery(datasetPoints: Array[OPTICSPoint], point: OPTICSPoint, eps: Float):Set[OPTICSPoint] = {

    var neighbors = ListBuffer[OPTICSPoint]()

    for (currentPoint <- datasetPoints) {

      if(Similarity.euclidean(point.dataPoint, currentPoint.dataPoint) <= eps) {
        neighbors += currentPoint
      }
    }

    return neighbors.toSet

  }

  def extractDBSCAN(pQueue: ListBuffer[OPTICSPoint], ei: Float, minPoints: Int): Unit = {
    var clusterID = -1

    for(point <- pQueue) {

      if(point.reachDistance>ei){

        if(point.coreDistance<=ei){

          clusterID = clusterID+1
          point.clusterID = clusterID

        } else {
          point.clusterID = -1
        }
      } else {
        point.clusterID = clusterID
      }
    }
  }
}

