/**
  * Created by pbanavara on 06/09/16.
  */
import scala.io.StdIn._

object SwapNode {

  def main(args: Array[String]) = {
    val noOfTestCases = readInt()
    (1 to noOfTestCases).map(b => constructTree(readLine().split(" ").toList.map(_.toInt)))
    val noOfSwaps = readInt()
    (1 to noOfSwaps).map(b => swapNodes(readInt))
  }

  case class node(value: Int, left: node, right: node)

  def constructTree(nodes: List[Int]) = {
    val root = node(1, node(nodes(0), null, null), node(nodes(1), null, null))
  }

  def swapNodes(depth: Int) :String = {
    ""
  }

}
