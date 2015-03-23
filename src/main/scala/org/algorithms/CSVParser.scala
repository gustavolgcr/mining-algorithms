package main.scala.org.algorithms

/**
 * Created by lsbd on 18/03/15.
 */
object CSVParser {
  def readFile(fileName: String, delimiter: Char = ';', dropNLines: Int = 1,
    dropNColumn: Int = 1) : List[Array[String]] = {
    var src = scala.io.Source.fromFile(fileName);
    var tempList = src.getLines().drop(dropNLines).map(_.split(delimiter)).
      map(_.dropRight(dropNColumn)).toList

    src.close()

    return tempList
  }
}
