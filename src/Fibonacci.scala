import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readInt

/**
  * Created by pbanavara on 26/08/16.
  */
object Fibonacci {

  def f(n:Int):List[Int] = {
    val mod = 100000007
    val lb = new ListBuffer[Int]()
    lb+=(0,1)
    (2 to n).map(a => lb+= (lb(a-1)+lb(a-2))%mod)
    lb.toList
  }
  def main(args: Array[String]){
    val n = readInt()
    val fb = f(10000)
    (1 to n).map{_ => val num =readInt(); println(fb(num))}
  }
}
