import scala.collection.immutable.{ Map => IMap
                                  , Set => ISet }

import scala.collection.mutable.{ Map => MMap
                                , Set => MSet
                                , LinkedList => MLinkedList }

object TP3 {
  def main(args: Array[String]) {
    println("Question 1:")
    println(toMap1("nantes") mkString "\n")
    println
    println("Question 2:")
    println(toMap2("nantes") mkString "\n")
    println
    val mll = MLinkedList(0, 0, 1, 0, 2, 3, 0, 0, 5, 0, 0)
    filterZeros(mll)
    println(mll)
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
  
  def toMap2(string: String): IMap[Char, ISet[Int]] = {
    def worker(
      str: String,
      map: IMap[Char, ISet[Int]],
      index: Int)
      : IMap[Char, ISet[Int]] = {
      if(str.isEmpty)
        map
      else
        worker(
          str.tail,
          map + (str.head -> (map get (str.head) match {
            case Some(s) => s + index
            case None    => ISet[Int](index)
          })),
          index + 1)
    }
    worker(string, IMap[Char, ISet[Int]](), 0)
  }
  
  def filterZeros(l: MLinkedList[Int]) {
    if(!l.isEmpty) {
      if(l.elem == 0) {
        if(l.next.isEmpty)
          l.next = l
        else {
          l.elem = l.next.elem
          l.next = l.next.next
        }
        filterZeros(l)
      } else {
        filterZeros(l.next)
      }
    } 
  }
}

