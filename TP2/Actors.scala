import scala.actors.{Actor, Exit}
import scala.actors.Actor._
import scala.actors.Futures._
import scala.util.Random

case class Work(slice: Array[Int])
case object Stop

object Actors {
  def main(args: Array[String]) {
    // taille du tableau dont on veut la moyenne et max des éléments
    val array = randomArray(11, 100)
    // nombre d'esclaves qu'on souhaite faire tourner
    val master = new Master(5)
    println("Les acteurs vont sommer le tableau suivant :")
    println(array mkString(" "))
    master.start
    println(master !? Work(array))
    master ! Stop
  }

  def randomArray(length: Int, max: Int): Array[Int] = {
    val generator = new Random
    new Array[Int](length) map(_ => generator nextInt max)
  }
}

class Master(slavesN: Int) extends Actor {
  def act () {
    val slaves = (1 to slavesN).map(_ => new Slave)
    slaves.foreach(s => {s start; this link s})
    Actor.loop {
      receive {
        case Work(array) =>
          val pieces = splitArray(array, slavesN)
          println("Chacun des %d esclaves va sommer un des tableaux suivants :"
              format slavesN)
          pieces foreach(x => println(x.mkString(" ")))
          val futures = (0 until slavesN) map(i => slaves(i) !! Work(pieces(i)))
          val results = awaitAll(3000, futures:_*)
          val ints = results collect {case Some(y:Int) => y}
          val sum = (0 /: ints)(_+_)
          println("Le maitre additionne toutes les sommes pour trouver : %d"
              format sum)
          print("Puis divise par le nombre d'éléments pour renvoyer la " +
              "moyenne : ")
          reply (sum / array.length)
        case Stop =>
          exit()
      }
    }
  }

  def splitArray(array: Array[Int], pieces: Int): Array[Array[Int]] = {
    val (piecesL, extras) = (array.length / pieces, array.length % pieces)
    val r = new Array[Array[Int]](pieces)
    var (length, start, end) = (0, 0, 0)
    for(i <- 0 until pieces) {
      length = if(i < extras) piecesL + 1 else piecesL
      end += length
      r(i) = array slice(start, end)
      start += length
    }
    r
  }

}

class Slave() extends Actor {
  def act () {
    trapExit = true
    Actor.loop {
      receive {
        case Work(slice) =>
          reply((slice :\ 0)(_+_))
        case Exit(_, _) =>
          exit()
      }
    }
  }
}
