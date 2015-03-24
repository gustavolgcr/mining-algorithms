package main.scala.org.algorithms


import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import java.io._
import scala.collection.mutable.HashMap
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

object CSVParser {

  def readFile(fileName: String, delimiter: Char = ';', dropNLines: Int = 1,
    dropNColumn: Int = 1) : List[Array[String]] = {
    val src = scala.io.Source.fromFile(fileName);
    val tempList = src.getLines().drop(dropNLines).map(_.split(delimiter)).
      map(_.dropRight(dropNColumn)).toList
    src.close()

    return tempList
  }

  //Manoel Rui
  def writeClustersFile(fileName: String, dataset: List[Array[String]], clusters: ListBuffer[ArrayBuffer[Int]], delimiter: Char = ';', noise: String = "-1") = {
    // Initializer all instances as a noise
    var clustersLabel = Array.fill(dataset.length)(noise)
    var writer = new PrintWriter(new File(fileName))

    // Make a map between one index of tuple in dataset to one cluster
    for(i <- 0 until clusters.length) {
      for(tuple <- clusters(i)) {
        clustersLabel(tuple) = i.toString
      }
    }

    for(i <- 0 until dataset.length) {
      for(attribute <- dataset(i)) {
        writer.write(attribute + delimiter)
      }

      writer.write(clustersLabel(i) + "\n")
    }

    writer.close()
  }

  //Iago Chaves
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
