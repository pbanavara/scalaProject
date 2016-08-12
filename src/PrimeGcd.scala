import scala.collection.immutable.SortedSet
import scala.io.StdIn._

/**
  * Created by pbanavara on 09/08/16.
  */
object PrimeGcd {

  def main(args: Array[String]) = {
    var limit = readInt()
    var listOfNumbers: List[String] = List()
    while(limit > 0) {
      val a = readLine()
      listOfNumbers = listOfNumbers :+ a
      limit = limit - 1
    }
    getPrime(listOfNumbers)
  }

  def getPrime(s: List[String]) :Unit = {

    def compareLists(a: List[(Int, Int)], b: List[(Int,Int)]) : List[(Int, Int)]= {
      /*
      val c = a.map( p => {
        b.filter(q => (p._1 == q._1))
      }).flatten
      */
      val set1 = SortedSet(a: _*)(Ordering by (_._1))
      val set2 = SortedSet(b: _*)(Ordering by (_._1))
      val c = set1 &~ set2 union set2 &~ set1
      val d = a diff c.toList
      val e = b diff c.toList
      d ::: e
    }

    val res = s.map(p => (p.split(" ")))
    //res.foreach(p => p.foreach(q => println(q)))
    val newRes = res.map(p => p.toList.grouped(2).toList)
    var aa = newRes.map( p => p.map(x => (x.head.toInt, x.tail.head.toInt)))
    var newA: List[List[(Int, Int)]] = List(List())
    for((x,i) <- aa.view.zipWithIndex) if (i < aa.length-1) {
      aa = List(compareLists(x, aa(i + 1)))
    }
    val nA = aa.flatten
    def remove(num: (Int, Int), list: List[(Int,Int)]) = list diff List(num)

    def compareElement(l:List[(Int, Int)], acc :List[(Int, Int)]) :List[(Int, Int)] = {
      if (l.isEmpty) acc else {
        val x = l.filter(p => l.head._1 == p._1)
        if (!x.isEmpty) {
          val y = x.reduceLeft((x,y) => (x._1, Math.min(x._2, y._2)))
          compareElement(l diff x,  y :: acc)
        } else {
          compareElement(l diff x, acc)
        }
      }
    }
    val newList = compareElement(nA, List())
    println(newList.sortBy(_._1).map(b => List(b._1,b._2)).flatten.mkString(" "))

  }

}
