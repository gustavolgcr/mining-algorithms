package main.scala.org.algorithms

/**
 * Created by gustavolgcr on 3/17/15.
 */
object test {
  def main(args: Array[String]) {
    var wines = CSVParser.readFile("datasets/winequality-red.csv")

    var it = 0
    for(wine <- wines) {
      println(wine(0))
    }
  }
}
