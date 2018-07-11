package Algorithm
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer
/**
  * Created by ken.J.Zheng on 7/11/2018.
  */
//advanced algorithm to calculate largest rectangle in histogram
//O(n)+
object RectangleAreaOfHistogramV2 extends App {
  case class HistogramBlock(startIndex:Int, endIndex:Int, rectangles:List[Rectangle], blockSummary:List[Int])
  case class Rectangle(startIndex:Int, endIndex:Int, heightIndex:Int, expandable:Boolean)

  //array is sorted from left to right, smallest to biggest
  def createHistogramBlock(blockSummary:List[Int], beginIndex:Int, endIndex:Int, data: Array[Int]): HistogramBlock = {
    var maxSize = 0
    var width = 0
    var rectangleWidth= 0
    var rectangleHeightIndex = 0

    val histogramBlock = if(blockSummary.size == 0){
      val baseRectangle = Rectangle(beginIndex, endIndex, beginIndex, true)
      new HistogramBlock(beginIndex, endIndex, List(baseRectangle), blockSummary)
    }
    else {
      for (i <- blockSummary.size - 1 to 0 by -1) {
        width = endIndex - blockSummary(i) + 1
        val height = data(blockSummary(i))
        val size = width * height

        if (size >= maxSize) {
          //left of max has potential to grow when join with histogram on the left
          maxSize = size
          rectangleHeightIndex = blockSummary(i)
          rectangleWidth = width
        }
      }
      val maxRectangle = Rectangle(endIndex - rectangleWidth + 1, endIndex, rectangleHeightIndex, false)
      val baseRectangle = Rectangle(beginIndex, endIndex, beginIndex, true)
      new HistogramBlock(beginIndex, endIndex, List(maxRectangle, baseRectangle), blockSummary)
    }
    histogramBlock
  }

  //join two rectangles into one
  def mergeHistogramBlock(left:HistogramBlock, right:HistogramBlock, data:Array[Int]):HistogramBlock={
    val listBuffer = new ListBuffer[Rectangle]()
    listBuffer ++= left.rectangles

    for(i<- 0 to right.rectangles.length-1){
      val rectangle = right.rectangles(i)
      if(rectangle.expandable){
        if(data(left.startIndex)>=data(rectangle.heightIndex)){
          val expandedRectangle = new Rectangle(left.startIndex,rectangle.endIndex,rectangle.heightIndex,true)
          listBuffer += expandedRectangle
        }
        else {
          breakable {
            for (i <- 0 to left.blockSummary.length - 1) {
              if (data(left.blockSummary(i)) >= data(rectangle.heightIndex)) {
                val unExpandableRectangle = new Rectangle(left.blockSummary(i),rectangle.endIndex,rectangle.heightIndex,false)
                listBuffer += unExpandableRectangle
                break()
              }
            }
          }
        }
      }
      else listBuffer += rectangle
    }

    new HistogramBlock(left.startIndex,right.endIndex,listBuffer.toList,left.blockSummary)

  }

  //when there is decline, we split
  //loop through each split, memorize each bin
  //return each split's max rectangle and base rectangle in the split
  def parseHistogram(data: Array[Int], pointer:Int): HistogramBlock = {
    val memory = ListBuffer[Int]() //value,continues count, minimum value in the list
    var ref = data(pointer)

    //climb high, memorize each index when value become bigger.
    for (i <- pointer+1 to data.length - 1) {
      if (data(i) < ref) { //split histogram into another block
      val rightHistogram = parseHistogram(data,i)
        val leftHistogram = createHistogramBlock(memory.toList,pointer,i-1,data)
        val rectangle = mergeHistogramBlock(leftHistogram,rightHistogram,data)
        return rectangle
      }
      else if(data(i) > ref) {
        memory.append(i)
        ref = data(i)
      }
    }
    createHistogramBlock(memory.toList,pointer,data.length-1,data)
  }

  def getMaxRectangle(rectangleList:List[Rectangle],data:Array[Int]): (Rectangle,Int) = {
    var maxSize = 0
    var maxRectangle = rectangleList(0)
    rectangleList.foreach(r => {
      val width = r.endIndex - r.startIndex + 1
      val height = data(r.heightIndex)
      val size = width * height

      if(size > maxSize) {
        maxRectangle = r
        maxSize = size
      }
    })

    (maxRectangle, maxSize)
  }

  val test1 = Array(1,2,3,3,3,4,5)
  val answer1 = parseHistogram(test1,0)
  val rectangle1 = getMaxRectangle(answer1.rectangles,test1)
  println(rectangle1)

  val test2 = Array(1,2,3,3,3,4,5,2,3,1,3,4,5)
  val answer2 = parseHistogram(test2,0)
  val rectangle2 = getMaxRectangle(answer2.rectangles,test2)
  println(rectangle2)

  val test3 = Array(1,2,3,3,3,4,5,4)
  val answer3 = parseHistogram(test3,0)
  val rectangle3= getMaxRectangle(answer3.rectangles,test3)
  println(rectangle3)

  val test4 = Array(1,2,3,3,3,4,5,4,3,2)
  val answer4 = parseHistogram(test4,0)
  val rectangle4= getMaxRectangle(answer4.rectangles,test4)
  println(rectangle4)

  val test5 = Array(1,2,3,3,3,4,5,0,4,3,2)
  val answer5 = parseHistogram(test5,0)
  val rectangle5= getMaxRectangle(answer5.rectangles,test5)
  println(rectangle5)

  val test6 = Array(6,6,6,6,6,6)
  val answer6 = parseHistogram(test6,0)
  val rectangle6= getMaxRectangle(answer6.rectangles,test6)
  println(rectangle6)

  val test7 = Array(6,2,5,4,5,1,6,7,8,9,10)
  val answer7 = parseHistogram(test7,0)
  val rectangle7= getMaxRectangle(answer7.rectangles,test7)
  println(rectangle7)

  //(Rectangle(6,10,6,false),30) ((startIndex,endIndex,heightIndex,false),size)
}
