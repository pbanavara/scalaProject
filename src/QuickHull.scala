import sun.font.TrueTypeFont

import scala.io.StdIn._

/**
  * Created by pbanavara on 02/08/16.
  */
object QuickHull {

  class HullPoint(val x:Int, val y:Int, val furthest: Int)

  def main(args: Array[String]) = {
    var limit = readInt()
    var listOfPoints:List[HullPoint] = List()
    while (limit > 0) {
      val a = readLine()
      val b = a.split(" ")
      val c:HullPoint = new HullPoint(b(0).toInt, b(1).toInt, 0)
      listOfPoints = listOfPoints :+ c
      limit -= 1
    }
    val list = listOfPoints.sortBy(_.x)
    val result = quickHull(list)
    println(perimeter(result))
  }

  def quickHull(s:List[HullPoint]) : List[HullPoint] = {
    val min = s.head
    val max = s.last
    val newS = s diff List(min, max)
    val s1 = turnLeft(newS, min, max)
    val s2 = newS diff s1
    val res1 = List(min, max) ::: findHull(s1, min, max, List())
    val res2 = findHull(s2, max, min, List())
    res1 ::: res2
  }

  def turnLeft(s:List[HullPoint], min: HullPoint, max: HullPoint) : List[HullPoint] = {
    //for (x <- s if (slope(x, min, max))) yield x
    def slope(P: HullPoint, A:HullPoint, B:HullPoint) : Boolean = {
       val slopeValue = ((B.x - A.x) * (P.y - A.y)) - ((B.y - A.y) * (P.x - A.x))
      val collinear = Math.abs(slopeValue) <= 1e-9
      val left = slopeValue > 0
      collinear || left
    }
    val dp = s.filter(p => (slope(p, min, max) == true))
    dp
  }

  def turnRight(s:List[HullPoint], min: HullPoint, max: HullPoint) : List[HullPoint] = {
    //for (x <- s if (slope(x, min, max))) yield x
    def slope(P: HullPoint, A:HullPoint, B:HullPoint) : Boolean = {
      val slopeValue = ((B.x - A.x) * (P.y - A.y)) - ((B.y - A.y) * (P.x - A.x))
      val collinear = Math.abs(slopeValue) <= 1e-9
      val right = slopeValue < 0
      collinear || right
    }
    val dp = s.filter(p => (slope(p, min, max) == true))
    dp
  }

  def findHull(s1: List[HullPoint], min: HullPoint, max:HullPoint, acc:List[HullPoint]) : List[HullPoint] = {
    def farthest(A: HullPoint, B:HullPoint, s:List[HullPoint]) : HullPoint = {
      val ABx = B.x-A.x;
      val ABy = B.y-A.y;
      val nS = s.map(b => new HullPoint(b.x, b.y, Math.abs(ABx*(A.y-b.y)-ABy*(A.x-b.x)))).sortBy(_.furthest)
      //println(nS.last.x + "," + nS.last.y)
      nS.last
    }
    if (s1.isEmpty) acc else {
      val farthestPoint = farthest(min, max, s1)
      val listWithoutFarthest = s1.dropWhile(p => (p.x == farthestPoint.x && p.y == farthestPoint.y))
      val leftS1 = turnLeft(listWithoutFarthest, min, farthestPoint)
      val insideTriangle = turnRight(listWithoutFarthest, min, farthestPoint).toSet intersect  turnLeft(listWithoutFarthest, min, max).toSet intersect  turnLeft(listWithoutFarthest, max, farthestPoint).toSet
      val newS1 = listWithoutFarthest diff insideTriangle.toList
      val rightS1 = turnRight(newS1, max, farthestPoint)
      val res1 = findHull(leftS1, min, farthestPoint, farthestPoint :: List())
      val res2 = findHull(rightS1, farthestPoint, max, farthestPoint :: List())
      res1 ::: res2
    }

  }

  def perimeter(s:List[HullPoint]):Double = {
    (s zip s.tail).map (b => Math.sqrt(Math.pow(b._2.x - b._1.x, 2) + Math.pow(b._2.y - b._1.y, 2))).sum + Math.sqrt(Math.pow(s.last.x - s.head.x, 2) + Math.pow(s.last.y - s.head.y, 2))
    //(s zip s.tail).map (b => Math.sqrt(Math.pow(b._2.x - b._1.x, 2) + Math.pow(b._2.y - b._1.y, 2))).sum
  }

}
