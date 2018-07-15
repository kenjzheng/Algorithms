package Algorithm

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.util.control.Breaks._

/**
  * Created by Ken.J.Zheng on 7/13/2018.
  * use RecursiveStaicaseV1 test case climbStaircase(10,5,ListBuffer[Int]()) steps calculation are repeated.
  * for example, 449->5,1,1,1,1,1, -> 1,1,1,1,1 are same as 62->1,1,2,1,1,1,1,1,1 last 5 steps when first 5 steps are calculated.
  */

object RecursiveStaircaseV2 extends App {
  val dict = Map[Int, List[List[Int]]]()
  var loopCount = 0

  def climbStaircase(stairs: Int, steps: Array[Int]): List[List[Int]] = {
    val key = stairs
    val accomplishedSteps = ListBuffer[List[Int]]()
    if (dict.contains(key)) {
      accomplishedSteps ++= dict(key)
    }
    else {
      breakable {
        for (i <- 0 to steps.length-1) {
          if (stairs - steps(i) > 0) {
            val r = climbStaircase(stairs - steps(i), steps)
            r.foreach(l => {
              accomplishedSteps += steps(i) :: l
            })
          }
          else if (stairs - steps(i) == 0) {
            loopCount += 1
            accomplishedSteps += List(steps(i))
            if (dict.contains(key)) {
              val existingList = dict(key)
              dict(key) = existingList ++ accomplishedSteps.toList
            }
            else {
              dict += (key -> accomplishedSteps.toList)
            }
          }
          else break()
        }
      }
    }
    accomplishedSteps.toList
  }

  val test1 = climbStaircase(10, Array(1,2,3,4,5))
  test1.zipWithIndex.foreach(t => println(t._2 + "->" + t._1.mkString(",")))
  println("loop count:" + loopCount)

  loopCount=0
  dict.clear()
  val test2 = climbStaircase(10, Array(1,3,5))
  test2.zipWithIndex.foreach(t => println(t._2 + "->" + t._1.mkString(",")))
  println("loop count:" + loopCount)
}
