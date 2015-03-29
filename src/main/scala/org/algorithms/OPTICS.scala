package main.scala.org.algorithms

import scala.collection.mutable.{ArrayBuffer, ListBuffer, PriorityQueue, HashMap}

class OPTICSPoint(_pointIndex: Int, _reachDist: Float = -1) {
  var pointIndex: Int = _pointIndex;
  var reachDist: Float = _reachDist;
}

class OPTICSPriorityQueue {
  var seeds = PriorityQueue[OPTICSPoint]()(OPTICSPointOrdering);

  def OPTICSPointOrdering = new Ordering[OPTICSPoint] {
    def compare(a : OPTICSPoint, b : OPTICSPoint) = b.reachDist.compareTo(a.reachDist)
  }

  // TODO: To fix ordering for float point between 0.0 and 1.0
  def updatePoint(point: Int, reachDistance: Float) = {
    this.seeds.find(x => Some(x.pointIndex) == Some(point)) match {
      case Some(p: OPTICSPoint) => {
        p.reachDist = reachDistance
      }
    }
  }
}

object OPTICS {
  var pointState:Array[PointState.PointState] = null;
  var clusters: ListBuffer[ArrayBuffer[Int]] = null;
  var reachabilityDistance:Array[Double] = null;

  object PointState extends Enumeration {
    type PointState = Value
    val Visited, Unvisited = Value
  }

  // TODO: The return will be a List of elements where each one will contains a point and reachability distance
  def apply(dataset: List[Array[Float]], eps: Float, minPts: Int):ListBuffer[ArrayBuffer[Int]] = {
    //clusters = new ListBuffer[ArrayBuffer[Int]]()
    reachabilityDistance = Array.fill(dataset.length)(-1.0f)

    for(it <- 0 until dataset.length) {
      var pointIndex = it
      var neighborPts = regionQuery(dataset, pointIndex, eps)

      pointState(pointIndex) = PointState.Visited
      // TODO: write this element in resultList

      if(coreDistance(dataset, neighborPts, pointIndex, eps, minPts) != -1) {

      }
    }

    return clusters
  }

  def update(dataset: List[Array[Float]], neighbors: Set[Int], pointIndex: Int, Seeds: PriorityQueue[Int], eps: Float, minPoints: Int) : Unit = {
    var coredist = coreDistance(dataset, neighbors, pointIndex, eps, minPoints)
    var neighborList = neighbors.toList

    for (it <- 0 until neighborList.length) {
      if(pointState(neighborList(it)) == PointState.Unvisited) {
        // pointState(neighborList(it)) = PointState.Visited ???
        var newReachDist = math.max(coredist, Similarity.euclidean(dataset(pointIndex), dataset(neighborList(it))))

        if(reachabilityDistance(neighborList(it)) == -1) {
          reachabilityDistance(neighborList(it)) = newReachDist

          // TODO: enqueue the element neighborList(it)
          // priorityQueue(neighborList(it))
        }

        // TODO: update the newReachDist of the object and inside of priority queue
        /* else if (newReachDist < reachabilityDistance(neighborList(it))) {
          reachabilityDistance(neighborList(it))) = newReachDist
          priorityQueue.update(neighborList(it), newReachDist)
        } */

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

        var it2 = distance.length

        while(Similarity.euclidean(dataset(pointIndex), dataset(neighborList(it))) < distance(it2)) {
          distance(it2+1) = distance(it2)
          distance(it2) = Similarity.euclidean(dataset(pointIndex), dataset(neighborList(it)))
          it2 = it2-1
        }
      }

      return distance(minPoints)
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
