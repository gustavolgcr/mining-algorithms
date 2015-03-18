package main.scala.org.algorithms

/**
 * Created by lsbd on 18/03/15.
 */
object CSVParser {
  def readFile(fileName: String) : List[Array[String]] = {
    var src = scala.io.Source.fromFile(fileName);
    var tempList = src.getLines().drop(1).map(_.split(";")).toList
    src.close()

    return tempList
  }
}
