/**
  * Created by pbanavara on 13/08/16.
  */
import scala.io.StdIn._

object ValidBst {

  case class Node (var data: Int, var left: Node, var right: Node) {
    override def toString = data.toString
  }

  case class BinarySearchTree (var root: Node) {
    def insert(data: Int): Unit = {
      if (root == null) {
        root = Node (data, null, null)
        return
      }
      var node = root
      while (node != null) {
        var res = data.compare(node.data)
        if (res == 0) return
        if (res < 0) {
          if (node.left == null) {
            node.left = Node(data, null ,null)
            return
          } else node = node.left

        } else {
          if (node.right == null) {
            node.right = Node(data, null, null)
            return
          } else node = node.right
        }
      }
    }
  }

  def preOrder(t: Node): List[Int] = t match {
    case null => Nil
    case Node(e, left, right) => e :: preOrder(left) ::: preOrder(right)
  }

  def main(args: Array[String]) = {
    var noOfTestCases = readInt()
    while (noOfTestCases > 0) {
      val noOfNodes = readInt()
      val nodes = readLine().split(" ").toList.map(p => p.toInt)
      var tree = BinarySearchTree(null)
      for( x <- nodes) tree.insert(x)
      if (nodes.mkString(" ") == preOrder(tree.root).mkString(" ")) println("YES") else println("NO")
      noOfTestCases -= 1
    }
  }

}
