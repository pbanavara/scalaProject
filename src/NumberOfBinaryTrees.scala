/**
  * Created by pbanavara on 01/09/16.
  */

import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readInt
object NumberOfBinaryTrees {

  def main(args: Array[String]) = {
    val n = readInt()
    for (i <- 1 to n) {
      println(findNumberOfBinaryTrees(readInt))
    }
  }

  var lb = new ListBuffer[Long]()
  for(i <- 1 to 10000) lb += 0

  def findNumberOfBinaryTrees(n: Long) :Long = {
    if (n <= 1) 1 else {
      if (lb(n.toInt) != 0) lb(n.toInt) else
      lb(n.toInt) = (1L to n).map(b => (findNumberOfBinaryTrees(b - 1)) * (findNumberOfBinaryTrees(n - b))).sum % 100000007
      lb(n.toInt)
    }
  }

}
