package main.scala.org.algorithms

import scala.collection.mutable.HashMap
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

/**
 * Created by lsbd on 18/03/15.
 */
object CSVParser {
  def readFile(fileName: String) : List[Array[String]] = {
    val src = scala.io.Source.fromFile(fileName);
    val tempList = src.getLines().drop(1).map(_.split(";")).map(_.dropRight(1)).toList

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

}
