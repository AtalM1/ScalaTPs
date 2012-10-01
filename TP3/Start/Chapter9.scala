object Chapter9 {
  def main(args: Array[String]) {
    val l = List(2, 8, 1, 9, 3, 7)
    println("Here is the l list we'll use to test our functions")
    println("  " + l)
    println
    println("9.1.1: insert without pattern matching on l")
    println("  " + isort(l))
    println
    println("9.2.1: a tail recursive version of length used on l")
    println("  " + tailRecLength(l))
    println
    println("9.4.1: squareList defined in two different ways used on l")
    println("  " + squareList1(l))
    println("  " + squareList2(l))
    println
    println("9.4.2: complexity differences")
    println("  Asymptotic complexity would be linear for the first definition" +
      " and quadratic for the second")
    println
    println("9.4.3: mapFun of squares and lengthFun on l")
    println("  " + mapFun(l, (x: Int) => x * x))
    println("  " + lengthFun(l))
  }
  
  def isort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) Nil
    else insert(xs.head, isort(xs.tail))

  def insert(x: Int, xs: List[Int]): List[Int] = {
    if(xs.isEmpty)
      List(x)
    else if(xs.head < x)
      xs.head :: insert(x, xs.tail)
    else
      x :: xs
  }
  
  def tailRecLength(xs: List[Int]): Int = {
    def worker(ys: List[Int], acc: Int): Int = ys match {
      case Nil =>
        acc
      case _ :: zs =>
        worker(zs, acc + 1)
    }
    worker(xs, 0)
  }
  
  def squareList1(xs: List[Int]): List[Int] = xs match {
    case List() =>
      Nil
    case y :: ys =>
      (y * y) :: squareList1(ys)
  }
  
  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x * x)

  def mapFun[A, B](xs: List[A], f: A => B): List[B] =
    (xs :\ List[B]()){ (x: A, ys: List[B]) => f(x) :: ys }

  def lengthFun[A](xs: List[A]): Int =
    (0 /: xs){ (s, _) => s + 1 }
}
