import scala.collection.immutable.SortedSet

/**
  * Created by pbanavara on 05/07/16.
  *
  *
  */
import scala.io.StdIn.{readInt,readLine}
object Intsets {

  abstract class IntSet {
    def contains(elem: Int): Boolean

    def incl(elem: Int): IntSet

    def union(other: IntSet): IntSet
  }

  object Empty extends IntSet {
    def contains(elem: Int): Boolean = false

    def incl(elem: Int): IntSet = new NonEmpty(elem, Empty, Empty)

    def union(other: IntSet) = other

  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean = {
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true
    }

    def incl(x: Int): IntSet = {
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this
    }

    def union(other: IntSet): IntSet = {
      ((left union right) union other) incl elem
    }

  }

  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) false
    if (chars.contains('(')) balance(chars.tail) else if (chars.contains(')')) true else false
  }

  /*
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 0 else if (coins.isEmpty) 0 else {
      val x = coins.head
    }
  }
  */

  def findExponent(x: Float): Float = {
    def fact(n: Int): Int = {

      def factAcc(n: Int, acc: Int): Int =
        if (n == 0) acc else factAcc(n - 1, n * acc)
      factAcc(n, 1)
    }

    // Compute and Return the value of e^x
    def expr(t: Float, incr: Int, acc: Float): Float = {
      if (incr == 10) acc
      else {
        //(1 + math.pow(expr(x,incr-1)/fact(incr-1).toFloat, incr -1)).toFloat
        expr(t, incr + 1, acc + (Math.pow(t, incr) / fact(incr)).toFloat)
      }
    }
    expr(x, 1, 0)
  }


  abstract class Nat {
    def isZero: Boolean

    def predecessor: Nat

    def successor: Nat

    def +(that: Nat): Nat

    def -(that: Nat): Nat
  }

  /*
  object Zero extends Nat {
    def isZero :Boolean = true
    def predecessor: Nat = throw new NumberFormatException("Not a negative number")
    def + (that:Nat): Nat = that
  }
   */

  def rotateString(s: List[Char], len: Int, acc: String): String = {
    if (len == 0) acc else rotateString(s.tail :+ s.head, len - 1, acc + (s.tail :+ s.head).mkString + " ")
  }

  class HullPoint(val x:Int, val y:Int)

  def pascalsTriangle(n: Int)  = {
    def doTriangle(n: Int, acc: List[Int]) : Unit = {
      if (n == 0) return else if (n == 1) {
        println(acc mkString (" "))
      } else {
        println(acc mkString (" "))
        doTriangle(n - 1, (1 :: (acc zip acc.tail).map(x => x._1 + x._2)) :+ 1)
      }
    }
    doTriangle(n, List(1))
  }

  def main(args: Array[String]) = {
    pascalsTriangle(5)
    val abc: List[Char] = List('(', 'a', 'b')
    //println(balance(abc))
    val i: Double = 5.0000
    //println(findExponent(i.toFloat))
    val inputString = "abcde"
    //println(rotateString(inputString.toList, inputString.length, ""))
    //println(stringMingle("abcde", "efghi", ""))
    println(isPrime(10))
    var limit = readInt()
    var listOfPoints:List[HullPoint] = List()
    while (limit > 0) {
      val a = readLine()
      val b = a.split(" ")
      val c:HullPoint = new HullPoint(b(0).toInt, b(1).toInt)
      listOfPoints :+ c
      limit -= 1
    }
    val list = listOfPoints sortBy(_.x)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => List(y) ++ init(ys)
  }

  def stringMingle(p: String, q:String, acc: String) :String = {
      if(p.length == 0 || q.length == 0) acc else {
        stringMingle(p.tail.mkString, q.tail.mkString, new StringBuilder(acc + p.head.toString + q.head.toString).toString())
      }
  }

  def perimeter(s:List[HullPoint]) : Double = {
    val x1 = s.head
    val x2 = s.tail.head
    val x3 = s.tail.tail.head

    val d1 = Math.sqrt(Math.pow(x1.x - x2.x, 2) + Math.pow(x1.y - x2.y, 2))
    val d2 = Math.sqrt(Math.pow(x2.x - x3.x, 2) + Math.pow(x2.y - x3.y, 2))
    val d3 = Math.sqrt(Math.pow(x3.x - x1.x, 2) + Math.pow(x3.y - x1.y, 2))

    (d1 + d2 + d3) / 3
  }

  def quickHull(s:List[HullPoint]) : List[HullPoint] = {
      val min = s.head
      val max = s.last
    s
  }

  def isPrime(n:Int) : Boolean = (2 until n) forall (d => n % d != 0)

  def scalarProduct(xs: List[Double], ys: List[Double]) : Double = {
    (for((x,y) <- xs zip ys) yield x * y).sum
  }

  def sumSegments(a: Array[Int], p:Double, s:Int, t:Int) :Int = {
    a.slice(s,t).reduceLeft((x,y) => Math.abs(Math.sqrt(Math.pow(x, 2) + Math.pow(y,2))).toInt)
  }
}
