package main.scala.org.algorithms

import scala.collection.mutable.ListBuffer

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

  def toFloat(dataset: List[Array[String]]) : List[Array[Float]] = {
    var newDataset = ListBuffer[Array[Float]]()

    for(i <- 0 until dataset.length) {
      newDataset += new Array[Float](dataset(i).length)
      for(j <- 0 until newDataset(i).length) {
        newDataset(i)(j) = dataset(i)(j).toFloat
      }
    }

    return newDataset.toList
  }
}
