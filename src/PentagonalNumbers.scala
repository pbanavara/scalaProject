import scala.io.StdIn._

/**
  * Created by pbanavara on 25/08/16.
  */
object PentagonalNumbers {

  def getPentagonalNumber(n: Long) :Long = {
    Math.pow(n,2).toLong + sum(n-1, 0)
  }

  def sum(n: Long, acc: Long) :Long = {
    if (n == 0) acc else
      sum(n-1, acc + n)
  }

  def main(args: Array[String]) = {
    var noOfTestCases = readInt()
    while (noOfTestCases > 0) {
      //println(getPentagonalNumber(readInt()))
      println(fibonacci(readInt).%((scala.math.pow(10,8).toInt + 7)))
      noOfTestCases -= 1
    }
  }

  var aMap = scala.collection.mutable.Map[BigInt, BigInt]()

  /*
    * Reject this fibonacci code. New code in a new file
   */

  def fibonacci(n:BigInt) :BigInt = {
    /*
      if (n == 0) 0 else if (n == 1) 1 else
        aMap.get(n) match {
      case Some(s) => s
      case None =>  {
        aMap(n) = fibonacci(n - 1) + fibonacci(n - 2)
        aMap(n)
      }
        }
        */
    def fibTr(n:BigInt, a1: BigInt, a2: BigInt) :BigInt = {
      if (n == 0) 0 else if( n == 1) a2 else fibTr(n-1, a2, a2 + a1)
    }
    fibTr(n, 0, 1)
  }
}
