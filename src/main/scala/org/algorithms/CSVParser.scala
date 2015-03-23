package main.scala.org.algorithms

import scala.collection.mutable.HashMap
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import scala.collection.mutable.ListBuffer

/**
 * Created by lsbd on 18/03/15.
 */
object CSVParser {

  def readFile(fileName: String, delimiter: Char = ';', dropNLines: Int = 1,
    dropNColumn: Int = 1) : List[Array[String]] = {
    val src = scala.io.Source.fromFile(fileName);
    val tempList = src.getLines().drop(dropNLines).map(_.split(delimiter)).
      map(_.dropRight(dropNColumn)).toList
    src.close()

    return tempList
  }


  def saveResult(filename: String, filename_original: String, map: HashMap[Int, Int]): Unit = {
    val src = scala.io.Source.fromFile(filename_original).getLines.toArray

    for ( (line, cluster) <- map ) {
      src(line) += ";" + cluster.toString
    }

    Files.write(Paths.get(filename), src.mkString("\n").getBytes(StandardCharsets.UTF_8))

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
