/**
  * Created by pbanavara on 02/08/16.
  */

import scala.io.StdIn.readInt
import scala.collection.breakOut

object Solution {
  def main(args: Array[String]) = {
    triangles(1)
  }

  def triangles(n: Int) :Unit = {
    def drawRow(row: Int, acc: List[Char]) : List[Char] = {
      if (row == 0) acc else {
        drawRow(row - 1, 'r' :: acc)
      }
    }
    def drawCol(col: Int, acc: List[Char]) : List[Char] = {
      if (col == 0) acc else {
        drawCol(col - 1, 'c' :: acc)
      }
    }
    println(List(drawRow(32,List('r')), drawCol(63,List('c'))))

  }
}

