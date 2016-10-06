/**
  * Created by pbanavara on 31/08/16.
  */
object Mathematics {

  def main(args: Array[String]) = {
    val n = readInt()
    for (i <- 1 to n) {
      val k = readLine().split(" ").toList.map(_.toInt)
      println(getSymmetry(k))
    }
  }

  def getSymmetry(s: List[Int]) :String = {
    val x1 = s.head
    val y1 = s.tail.head
    val mx1 = s.tail.tail.head
    val my1 = s.tail.tail.tail.head

    val x2 = 2 * mx1 - x1
    val y2 = 2 * my1 - y1
    new StringBuilder().append(x2.toString).append(" ").append(y2.toString).toString()
  }

  def aCount(a: List[Int], acc:Int) :Int = {
    if (a.isEmpty) acc else aCount(a.tail, acc + 1)
  }

}
