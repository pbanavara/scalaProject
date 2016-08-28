import scala.io.StdIn._

/**
  * Created by pbanavara on 17/08/16.
  */
object SubsetSum {

  def main(args: Array[String]) = {
    val size = readInt()
    val mainList = readLine.split(" ").toList.map(_.toInt)
    var noOfTestCases = readInt()
    while (noOfTestCases > 0) {
      val targetSum = readInt()
      println(findSubsets(mainList, targetSum))
      noOfTestCases -= 1
    }
  }

  def findSubsets(l: List[Int], target: Int) :Int = {
    val size = 0

    def rotateRight(seq: List[Int], i: Int): List[Int] = {
      val size = seq.size
      (seq.drop(size - (i % size)) ++ seq.take(size - (i % size))).toList
    }

    def recursivelyComputeElements(l: List[Int], windowSize: Int, acc: List[List[Int]]) :List[List[Int]] = {
      if (windowSize == 1) acc else {
        recursivelyComputeElements(l, windowSize - 1, acc ::: l.sliding(windowSize - 1).toList)
      }
    }
    var c = rotateRight(l,1)
    var x : List[List[List[Int]]] = List(List(List()))
    /*
    while (c != l) {
      val d = recursivelyComputeElements(c, c.length, List(List()))
      x = x:+ d
      c = rotateRight(c, 1)
    }
    */
    //val y = x.flatten.distinct.filter( p => p.sum >= target).sortBy(_.length)
    val y = l.toSet[Int].subsets.map(_.toList).toList.filter(p => p.sum >= target).sortBy(_.length)
    if (y.isEmpty) -1 else y.head.length
  }

}
