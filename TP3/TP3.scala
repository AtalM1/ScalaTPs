import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.collection.immutable.{Map => IMap, Set => ISet}

object TP3 {
  def main(args: Array[String]) {
    println(toMap1("nantes") mkString "\n")
  }
  
  def toMap1(string: String): MMap[Char, MSet[Int]] = {
    val map = MMap[Char, MSet[Int]]()
    for((c, i) <- string.zipWithIndex)
      map get c match {
        case Some(s) =>
          s add i
        case None    =>
          map.put(c, MSet(i))
      }
    map
  }
  
}

