package main.scala.org.algorithms

<<<<<<< HEAD
import scala.collection.mutable.HashMap
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
=======
import scala.collection.mutable.ListBuffer
>>>>>>> 2aef93f009d20ca051967bf90f0fdb6ebe2f4f65

/**
 * Created by lsbd on 18/03/15.
 */
object CSVParser {
<<<<<<< HEAD
  def readFile(fileName: String) : List[Array[String]] = {
    val src = scala.io.Source.fromFile(fileName);
    val tempList = src.getLines().drop(1).map(_.split(";")).map(_.dropRight(1)).toList
=======
  def readFile(fileName: String, delimiter: Char = ';', dropNLines: Int = 1,
    dropNColumn: Int = 1) : List[Array[String]] = {
    var src = scala.io.Source.fromFile(fileName);
    var tempList = src.getLines().drop(dropNLines).map(_.split(delimiter)).
      map(_.dropRight(dropNColumn)).toList
>>>>>>> 2aef93f009d20ca051967bf90f0fdb6ebe2f4f65

    src.close()

    return tempList
  }

<<<<<<< HEAD
  def saveResult(filename: String, filename_original: String, map: HashMap[Int, Int]): Unit = {
    val src = scala.io.Source.fromFile(filename_original).getLines.toArray

    for ( (line, cluster) <- map ) {
      src(line) += ";" + cluster.toString
    }

    Files.write(Paths.get(filename), src.mkString("\n").getBytes(StandardCharsets.UTF_8))

  }

=======
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
>>>>>>> 2aef93f009d20ca051967bf90f0fdb6ebe2f4f65
}
