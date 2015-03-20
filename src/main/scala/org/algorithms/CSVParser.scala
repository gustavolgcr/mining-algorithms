package main.scala.org.algorithms

/**
 * Created by lsbd on 18/03/15.
 */
object CSVParser {
  def readFile(fileName: String) : List[Array[String]] = {
    var src = scala.io.Source.fromFile(fileName);
    var tempList = src.getLines().drop(1).map(_.split(";")).map(_.dropRight(1)).toList

//    for(i <- 0 until tempList.length) {
//      for(j <- 0 until tempList(i).length) {
//        print(tempList(i)(j) + "; ")
//      }
//      println("")
//    }

    src.close()

    return tempList
  }
}
