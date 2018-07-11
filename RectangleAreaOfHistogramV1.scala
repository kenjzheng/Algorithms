package Algorithm
import scala.util.control.Breaks._
/**
  * Created by Ken.J.Zheng on 7/6/2018.
  */
//naive algorithm to calculate largest rectangle in histogram
//worse case O(n*n) when all values are same

object RectangleAreaOfHistogramV1 extends App {
  def getLeftCount(array:Array[Int], startIndex:Int, compareValue:Int):Int = {
    var count = 0
    breakable {
      for(i <- startIndex to 0 by -1){
        if(array(i)>=compareValue) count += 1
        else break()
      }
    }

    count
  }

  def getRightCount(array:Array[Int], startIndex:Int, compareValue:Int):Int = {
    var count = 0
    breakable {
      for(i <- startIndex to array.length-1){
        if(array(i)>=compareValue) {
          count += 1
        }
        else break()
      }
    }

    count
  }

  def getMaxRectangleSize(histogramArray: Array[Int]): (Int,Int,Int,Int)= {
    var max = (0,0,0,0)
    for(i <- 0 to histogramArray.length-1){
      var right = 0
      var left = 0
      if(i==0){
        right = getRightCount(histogramArray,i+1,histogramArray(i))
      }
      else if(i==histogramArray.length-1){
        left = getLeftCount(histogramArray,i-1,histogramArray(i))
      }
      else {
        right = getRightCount(histogramArray,i+1,histogramArray(i))
        left = getLeftCount(histogramArray,i-1,histogramArray(i))
      }

      val rectangleSize = histogramArray(i)*(1+right+left)
      if(rectangleSize> max._1)
        max = (rectangleSize,i,left,right)

      //println((rectangleSize,i,left,right))
    }
    max
  }

  val test1 = Array(6,2,5,4,5,1,6)
  println(getMaxRectangleSize(test1))

  val test2 = Array(6,2,5,4,5,1,6,7,8,9,10)
  println(getMaxRectangleSize(test2))

  val test3 = Array(6,6,6,6,6,6)
  println(getMaxRectangleSize(test3))

  val test4 = Array(1,2,3,4,5,6,5,4,3,2,1)
  println(getMaxRectangleSize(test4))

  //(30,6,0,4) (size, index, left of index, right of index)
}
