package main.scala.org.algorithms

/**
 * Created by gustavolgcr on 3/17/15.
 */
object test {
  def main(args: Array[String]) {

    var wines = CSVParser.readFile("datasets/winequality-red.csv")

    var normalizedWines = Normalization.featureScaling(wines, 0, 100)

    var answer = KMeans(normalizedWines, 6, 0.1f)

    for(i <- 0 until answer.length) {
      println("Cluster " + i)
      for(j <- 0 until answer(i).points.length-1) {
        for(k <- 0 until answer(i).points(j).attributes.length) {
          print(answer(i).points(j).attributes(k) + "; ")
        }
        print("\n")

      }
    }

//    println(answer)

  }
}
