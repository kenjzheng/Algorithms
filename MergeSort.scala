package Algorithm
import scala.collection.mutable.ArrayBuffer
/**
  * Created by ken.j.zheng on 6/3/2018.
  */
object MergeSort extends App {
  def mergeSort(array: Array[Int]):Array[Int] = {
    //split in half
    var left = array.take(array.length/2)
    var right = array.drop(array.length/2)

    left = if(left.length>1){
      mergeSort(left)
    }
    else left

    right = if(right.length>1){
      mergeSort(right)
    }
    else right

    var sortedArray = new ArrayBuffer[Int]()

    var leftCheckpoint = 0
    var rightCheckpoint = 0
    println("left->"+left.mkString(","),"right->"+right.mkString(","))

    while (leftCheckpoint<left.length || rightCheckpoint<right.length) {
      if(leftCheckpoint<left.length && rightCheckpoint==right.length){ //run out of right
        val tmp = left.drop(leftCheckpoint)
        sortedArray = sortedArray ++ tmp
        leftCheckpoint += tmp.length
      }
      else if(rightCheckpoint<right.length && leftCheckpoint==left.length){ //run out of left
        val tmp = right.drop(rightCheckpoint)
        sortedArray = sortedArray ++ tmp
        rightCheckpoint += tmp.length
      }
      else if(leftCheckpoint<left.length && left(leftCheckpoint)<=right(rightCheckpoint)){
        sortedArray.append(left(leftCheckpoint))
        leftCheckpoint +=1
      }
      else if (rightCheckpoint<right.length) {
        sortedArray.append(right(rightCheckpoint))
        rightCheckpoint +=1
      }
    }
    //println("sorted->",sortedArray.mkString(","))
    sortedArray.toArray
  }

  //used for debugging, even numbers
  val test1 = Array(1,4)
  println(mergeSort(test1).mkString(","))

  val test2 = Array(4,1)
  println(mergeSort(test2).mkString(","))

  val test3 = Array(3,1,7,8,2,4)
  println(mergeSort(test3).mkString(","))

  val test = Array(3,4,5,11,22,1,3,6,7,9,22,2,1,5,8,16,7,55,100,22,33,44,8,2,1)
  println(mergeSort(test).mkString(","))
}
