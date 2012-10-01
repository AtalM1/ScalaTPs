import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.collection.immutable.{Map => IMap, Set => ISet}

object TP3 {
  def main(args: Array[String]) {
    println(toMap1("nantes").mkString("\n"))
  }
  
  def toMap1(string: String): MMap[Char, MSet[Int]] = {
    val map = MMap[Char, MSet[Int]]()
    for(i <- 0 until string.length) {
      map.getOrElse(string(i), MSet()).add(i)
    }
    map
  }
  
}

