package main.scala.org.algorithms

import scala.collection.mutable.{ArrayBuffer, ListBuffer, PriorityQueue, HashMap}

class OPTICSPoint(_pointIndex: Int, _reachDistance: Float = -1, _coreDistance: Float = -1) {
  var pointIndex: Int = _pointIndex;
  var reachDistance: Float = _reachDistance;
  var coreDistance: Float = _coreDistance;
}

class OPTICSPriorityQueue {
  var seeds = PriorityQueue[OPTICSPoint]()(OPTICSPointOrdering);

  def OPTICSPointOrdering = new Ordering[OPTICSPoint] {
    def compare(a : OPTICSPoint, b : OPTICSPoint) = b.reachDistance.compareTo(a.reachDistance)
  }

  def updatePoint(point: Int, reachDistance: Float) = {
    this.seeds.find(x => Some(x.pointIndex) == Some(point)) match {
      case Some(p: OPTICSPoint) => {
        p.reachDistance = reachDistance
        // Creates a new queue and force the function to compare and update values
        this.seeds = this.seeds.clone()
      }
    }
  }
}

object OPTICS {
  var pointState:Array[PointState.PointState] = null;
  var pQueue:OPTICSPriorityQueue = null;
  var datasetPoints:Array[OPTICSPoint] = null;

  object PointState extends Enumeration {
    type PointState = Value
    val Visited, Unvisited = Value
  }

  def apply(dataset: List[Array[Float]], eps: Float, minPts: Int) : ListBuffer[OPTICSPoint] = {
    pointState = Array.fill(dataset.length)(PointState.Unvisited)
    datasetPoints = new Array[OPTICSPoint](dataset.length)
    pQueue = new OPTICSPriorityQueue
    var resulList = new ListBuffer[OPTICSPoint]

    // Initialize OPTICPoint's array
    for(pointIndex <- 0 until datasetPoints.length) {
      datasetPoints(pointIndex) = new OPTICSPoint(pointIndex, -1)
    }

    for(point <- datasetPoints) {
      if(pointState(point.pointIndex) == PointState.Unvisited) {
        var neighborPts = regionQuery(dataset, point.pointIndex, eps)

        pointState(point.pointIndex) = PointState.Visited
        resulList += point

        point.coreDistance = coreDistance(dataset, neighborPts, point.pointIndex, eps, minPts)
        if (point.coreDistance != -1) {
          update(dataset, neighborPts, point, pQueue, eps, minPts)

          while(!pQueue.seeds.isEmpty) {
            var pointNeighbor = pQueue.seeds.dequeue()
            var newNeighbors = regionQuery(dataset, pointNeighbor.pointIndex, eps)
            pointNeighbor.coreDistance = coreDistance(dataset, newNeighbors, pointNeighbor.pointIndex, eps, minPts)
            resulList += pointNeighbor

            if(pointNeighbor.coreDistance != -1) {
              update(dataset, newNeighbors, pointNeighbor, pQueue, eps,minPts)
            }
          }
        }
      }
    }

    return resulList
  }

  def update(dataset: List[Array[Float]], neighbors: Set[Int], point: OPTICSPoint, Seeds: OPTICSPriorityQueue, eps: Float, minPoints: Int) : Unit = {
    var coredist = point.coreDistance
    var neighborList = neighbors.toList

    for (it <- 0 until neighborList.length) {
      if(pointState(neighborList(it)) == PointState.Unvisited) {
        var newReachDist = math.max(coredist, Similarity.euclidean(dataset(point.pointIndex), dataset(neighborList(it))))

        if(datasetPoints(neighborList(it)).reachDistance == -1) {
          datasetPoints(neighborList(it)).reachDistance = newReachDist
          pQueue.seeds.enqueue(datasetPoints(neighborList(it)))
        } else if (newReachDist < datasetPoints(neighborList(it)).reachDistance) {
          datasetPoints(neighborList(it)).reachDistance = newReachDist
          pQueue.updatePoint(neighborList(it), newReachDist)
        }
      }
    }
  }

  //Double check this method
  def coreDistance(dataset: List[Array[Float]], neighbors: Set[Int], pointIndex: Int, eps: Float, minPoints: Int) : Float = {
    var distance = ListBuffer[Float]()
    var neighborList = neighbors.toList

    if(neighborList.length < minPoints) {
      return -1

    } else {
      distance += Similarity.euclidean(dataset(pointIndex), dataset(neighborList(0)))

      for(it <- 1 until neighborList.length) {

        var lastIndex = distance.length

        var tempEuclideanDistance = Similarity.euclidean(dataset(pointIndex), dataset(neighborList(it)))
        distance += tempEuclideanDistance
        while(lastIndex >= 0 && tempEuclideanDistance < distance(lastIndex)) {
          distance(lastIndex + 1) = distance(lastIndex)
          distance(lastIndex) = tempEuclideanDistance
          lastIndex = lastIndex - 1
        }
      }

      return distance(minPoints-1)
    }
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
