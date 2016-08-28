/**
  * Created by pbanavara on 19/08/16.
  */

import scala.io.StdIn.readLine
object StringCompress {

  def main(args: Array[String]) = {
    val x = readLine
    println(compress(' ', x, "", 1))
  }

  def compress(firstChar: Char, input: String, acc:String, count: Int) :String = {
    if (input.length == 1) {
      if (count > 1) new StringBuilder(acc.mkString).append(input.last + count.toString).toString() else acc.mkString + input.last
    } else if (input.head != input.tail.head) {
      if (count == 1) compress(input.tail.head, input.tail, new StringBuilder(acc).append(input.head).toString, 1)
      else
        compress(input.tail.head, input.tail, new StringBuilder(acc).append(input.head).append(count.toString).toString, 1)
    } else compress(input.head, input.tail, acc, count + 1)
  }

}
