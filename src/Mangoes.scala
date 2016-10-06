/**
  * Created by pbanavara on 07/09/16.
  */

import scala.io.StdIn._
import scala.util.Random

object Mangoes {

  def main(args: Array[String]) = {
    val initArray = readLine.split(" ").map(_.toLong)
    val noOfFriends = initArray(0)
    val noOfMangoes = initArray(1)
    val apetiteArray = readLine.split(" ").map(_.toLong)
    val happinessArray = readLine.split(" ").map(_.toLong)
    getNoofFriends(apetiteArray, happinessArray, noOfFriends, noOfMangoes)
  }

  def getNoofFriends(apetiteArray: Array[Long], happinessArray: Array[Long], noOfFriends: Long, noOfMangoes: Long) = {
    //Choose a random number of people between 0 and noOfFriends
    // Test the total number of mangoes possible for that group
    def createLists(noOfFriends: Int, beginIndex: Int, endIndex: Int, acc: List[List[Int]]) :List[List[Int]] = {
      if (endIndex == noOfFriends) acc else
       createLists(noOfFriends , beginIndex, endIndex + 1, Random.shuffle((1 to noOfFriends).slice(beginIndex, endIndex).toList) :: acc)

    }
    def createListsTwo(noOfFriends: Int, beginIndex: Int, endIndex: Int, acc: List[List[Int]]) :List[List[Int]] = {
      var temp = List[List[Int]]()
      for( i <- 1 to noOfFriends) {
        for (j <- 1 to endIndex) {
          temp = temp :+ Random.shuffle((1 to noOfFriends).slice(beginIndex, j).toList)
        }
      }
      temp.toSet.toList
    }

    def createListsThree(noOfFriends: Long) :List[List[Long]] = {
      (1L to noOfFriends).toSet.subsets.map(_.toList).toList
    }

    //val x = compareWithTotalMangoes(createListsThree(noOfFriends), apetiteArray, happinessArray, noOfMangoes).filter(p => p._2 < noOfMangoes).sortBy(_._2).last._1
    val x = compareWithTotalMangoesOpt(noOfFriends, apetiteArray, happinessArray, noOfMangoes)
    println(x)
  }

  def compareWithTotalMangoes(input: List[List[Long]], apetiteArray: Array[Long], happinessArray: Array[Long], noOfMangoes: Long) :List[(Int,Long)] = {
    input.map( child => {
      (child.length -> child.map( b => {
        apetiteArray(b.toInt - 1) + (child.length - 1) * happinessArray(b.toInt - 1)
      }).sum)
    })
  }

  def compareWithTotalMangoesOpt(noOfFriends: Long, apetiteArray: Array[Long], happinessArray: Array[Long], noOfMangoes: Long) : (Int, Long) = {
    (1L to noOfFriends).toSet.subsets.map(_.toList).map( child => {
      (child.length -> child.map( b => {
        apetiteArray(b.toInt - 1) + (child.length - 1) * happinessArray(b.toInt - 1)
      }).sum)
    }).filter(p => p._2 <= noOfMangoes).toList.sortBy(_._2).last
  }

}
