package main.scala.org.algorithms

import scala.collection.mutable.{ArrayBuffer, ListBuffer, PriorityQueue, HashMap}

class OPTICSPoint(_pointIndex: Int, _reachDistance: Float = -1f, _coreDistance: Float = -1f,
                  _dataPoint: Array[Float] = null, _isProcessed: Boolean = false, _clusterId: Int = -1) {

  var pointIndex: Int = _pointIndex
  var reachDistance: Float = _reachDistance
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

//      for each unprocessed point p of DB
      if(point.isProcessed == false) {

//        N = getNeighbors(p, eps)
        var neighborhoodPoints = regionQuery(datasetPoints, point, eps)

//        mark p as processed
        point.isProcessed = true
        point.coreDistance = coreDistance(datasetPoints, neighborhoodPoints, point, eps, minPts)
        resulList += point

        if (point.coreDistance != -1f) {

          update(datasetPoints, neighborhoodPoints, point, priorityQueue, eps, minPts)

          while(!priorityQueue.seeds.isEmpty) {

            var pointNeighbor = priorityQueue.seeds.dequeue()

            neighborhoodPoints = regionQuery(datasetPoints, pointNeighbor, eps)
            pointNeighbor.isProcessed = true
            pointNeighbor.coreDistance = coreDistance(datasetPoints, neighborhoodPoints, pointNeighbor, eps, minPts)
            resulList += pointNeighbor

            if(pointNeighbor.coreDistance != -1f) {

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


    for (neighborhoodIterator <- neighborhoodPoints) {
      if(neighborhoodIterator.isProcessed == false) {

        var newReachDistance = math.max(coreDistance, Similarity.euclidean(neighborhoodIterator.dataPoint, point.dataPoint))

        if(neighborhoodIterator.reachDistance == -1f) {

          neighborhoodIterator.reachDistance = newReachDistance

          priorityQueue.seeds.enqueue(neighborhoodIterator)

        } else {

          if (newReachDistance < neighborhoodIterator.reachDistance) {

            println("newReachDistance: " + newReachDistance)
            println("neighborhoodIterator.reachDistance: " + neighborhoodIterator.reachDistance)

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

    if(neighborhoodPoints.size < minPoints) {

      return -1f

    } else {

      for(iterator <- neighborhoodPoints) {

        distance.enqueue(Similarity.euclidean(point.dataPoint, iterator.dataPoint))

      }

    }

    for(it <- 0 until minPoints) {
      distance.dequeue()
    }

    return distance.dequeue()

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

