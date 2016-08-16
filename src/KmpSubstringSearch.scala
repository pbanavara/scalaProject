import scala.io.StdIn._

/**
  * Created by pbanavara on 16/08/16.
  */
object KmpSubstringSearch {

  def kmpSearch(s: String, w: String) :String = {
    val sL = s.toList
    val wL = w.toList

    def compareString(mainString: List[Char], searchString: List[Char], acc: Int) : Int  = {
      if (mainString.isEmpty) acc
      else {
        if (!searchString.isEmpty) {
          if (searchString.head == mainString.head) {
            compareString(mainString.tail, searchString.tail, sL.indexOf(mainString.head))
          } else {
            compareString(mainString.tail, wL, -1)
          }
        } else acc
      }
    }
    val x = compareString(sL, wL, -1)
    if (x > -1) "YES" else "NO"
  }

  def main(args: Array[String]) = {
    var noOfTestCases = readInt()
    while (noOfTestCases > 0) {
      val mainString = readLine
      val searchString = readLine
      println(kmpSearch(mainString, searchString))
      noOfTestCases -= 1
    }
  }

}
