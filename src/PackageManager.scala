import options.InputPackage._
import options.PackageLibrary._

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object PackageManager {
  def main(args: Array[String]): Unit = {
    loadPackageLibrary("src/PackageLibraryV.txt")

    checkForCycles()

    loadInputPackage("src/InputPackageV.txt")

    getMissingDependencies()
    println
    getExcessDependencies()
    println
    getDependencyTree("hbase")
    getDependencyTree("apache-commons")
  }

  def checkForCycles(): Unit = {

    def dfs(vertexAndAdj: (String, List[String]), visited: List[String] = List[String]()): Boolean = {
      //Depth-first search + cycle detection
      if (visited.contains(vertexAndAdj._1))
        true
      else
        vertexAndAdj._2.exists(
          el =>
            graph.get(el) match {
              case Some(vertices) =>
                dfs((el, vertices), visited :+ vertexAndAdj._1)
              case None =>
                false
            }
        )
    }

    if (graph.exists(dfs(_))) {
      throw new Exception("Circular dependency detected")
    }
  }

  def getMissingDependencies(): Unit = {
    val missingDependencies = getMissingDependenciesList()

    if (missingDependencies.nonEmpty) {
      println("Missing dependencies:")
      missingDependencies.foreach(el => println(inputPackageName + " -> " + el.mkString(" -> ")))
    } else
      println("No missing dependencies found")
  }

  def getMissingDependenciesList(): List[List[String]] = {

    val subgraphVertices: ListBuffer[String] = ListBuffer[String]()

    def dfsFindMissing(vertexAndAdj: (String, List[String]), resultList: ListBuffer[List[String]], visited: ListBuffer[String] = ListBuffer[String]()): Unit = {
      //Depth-first search + missing dependencies list building
      for (el <- vertexAndAdj._2) {

        if (!inputDependencies.contains(el) && !resultList.exists(list => list.last == el)) {
          resultList += (visited :+ el).toList
          subgraphVertices += el
        }
        visited --= visited.takeRight(visited.length - 1 - visited.indexOf(vertexAndAdj._1))
        val vertices = graph.get(el)
        if (vertices.isDefined) {
          dfsFindMissing((el, vertices.get), resultList, visited :+ el)
        }

      }
    }

    val resultList = ListBuffer[List[String]]()
    graph.get(inputPackageName) match {
      case Some(vertices) =>
        dfsFindMissing((inputPackageName, vertices), resultList)
      case None =>
        throw new Exception("No dependent packages for " + inputPackageName)
    }

    //Version control
    val subgraphVerticesNames = subgraphVertices.distinct.map(el => el.split(":"))
    for (el <- subgraphVerticesNames) {
      if (el.length > 1) {
        for (compEl <- subgraphVerticesNames) {
          if (compEl.length > 1 && el(1) != compEl(1) && el(0) == compEl(0)) {
            throw new Exception("There are version conflicts in " + inputPackageName + " dependencies:\n" +
              " - " + getDependencyTreeList(el(0) + ":" + el(1)).mkString(" -> ") + "\n" +
              " - " + getDependencyTreeList(compEl(0) + ":" + compEl(1)).mkString(" -> "))
          }
        }
      }
    }

    resultList.toList
  }

  def getDependencyTree(lib: String): Unit = {
    if (lib == inputPackageName)
      println(lib)
    else {
      val dependencyTreeList = getDependencyTreeList(lib)
      if (dependencyTreeList.nonEmpty)
        println(inputPackageName + " -> " + getDependencyTreeList(lib).mkString(" -> "))
      else
        println("No " + lib + " dependency found")
    }
  }

  def getExcessDependencies(): Unit = {
    val resultList = ListBuffer[String]()
    for (dependency <- inputDependencies) {
      if (getDependencyTreeList(dependency).isEmpty)
        resultList += dependency
    }
    if (resultList.nonEmpty) {
      println("Excess dependencies:")
      resultList.foreach(el => println("- " + el))
    } else
      println("No excess dependencies found")
  }

  def getDependencyTreeList(lib: String): List[String] = {

    def dfsFind(vertexAndAdj: (String, List[String]), visited: ListBuffer[String]): Unit = {
      //Depth-first search until lib found
      breakable {
        for (el <- vertexAndAdj._2) {
          if (visited.contains(lib)) {
            break
          }
          visited --= visited.takeRight(visited.length - 1 - visited.indexOf(vertexAndAdj._1))
          if (el == lib) {
            visited += el
            break
          } else {
            val vertices = graph.get(el)
            if (vertices.isDefined) {
              dfsFind((el, vertices.get), visited += el)
            }
          }
        }
      }
    }

    val visited_vertices: ListBuffer[String] = ListBuffer[String]()
    graph.get(inputPackageName) match {
      case Some(vertices) =>
        dfsFind((inputPackageName, vertices), visited_vertices)
      case None =>
        throw new Exception("No dependant packages for " + inputPackageName)
    }
    visited_vertices.toList
  }
}
