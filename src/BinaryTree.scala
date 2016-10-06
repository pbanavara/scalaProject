/**
  * Created by pbanavara on 31/08/16.
  */

abstract class BinaryTree {
  def insert(data: Int): BinaryTree
}

class NonEmpty(data: Int, left: BinaryTree, right: BinaryTree) extends BinaryTree {
  override def insert(newData: Int) :BinaryTree = {
    if(newData < data) new NonEmpty(data, left.insert(newData), right)  else if (newData > data) new NonEmpty(data, left, right.insert(newData))
    else this
  }
  def isEmpty = false
}

class Empty extends BinaryTree {
  def insert(newData: Int) = new NonEmpty(newData, new Empty, new Empty)
}

object BinaryTreeOps {

  def main(ars: Array[String]) = {
    val a = List(10,1,0,3,4,5,6)
    //for (x <- a) new NonEmpty(null,null,null).insert(x)
  }
}


