package options

import scala.io.Source

package object PackageLibrary {
  var graph: Map[String, List[String]] = Map[String, List[String]]()

  def loadPackageLibrary(source: String): Unit = {
    val file = Source.fromFile(source)
    try {
      for (line <- file.getLines) {
        //println(line)
        val tmpArr = line.split(" -> ")
        val rightLibs = tmpArr(1).split("[\\[\\], ]").filterNot(_.isEmpty).toList
        if (rightLibs.nonEmpty)
          graph += (tmpArr(0) -> rightLibs)
      }
    } finally {
      file.close
    }
  }
}

package object InputPackage {
  var inputPackageName: String = ""
  var inputDependencies: List[String] = List[String]()

  def loadInputPackage(source: String): Unit = {
    val file = Source.fromFile(source)
    val firstLine = {
      val line = file.bufferedReader.readLine
      file.close
      line
    }
    inputPackageName = firstLine.split(" -> ")(0)
    inputDependencies = firstLine.split(" -> ")(1).split("[\\[\\], ]").filterNot(_.isEmpty).toList

  }
}


