package Algorithm

/**
  * Created by ken.j.zheng on 6/3/2018.
  */
object InsertionSort extends App {
  def insertionSort(array: Array[Int],index: Int, checkPoint: Int): Array[Int] = {
    val sortedArray = if(index<array.length) {
      if (index > 0 && array(index - 1) > array(index)) {
        val tmp = array(index)
        array(index) = array(index - 1)
        array(index - 1) = tmp

        if (index > 1 && array(index - 2) > array(index - 1)) {
          insertionSort(array, index - 1, checkPoint)
        }
        else {
          insertionSort(array, checkPoint + 1, checkPoint+1) //restart from previous checkpoint
        }
      }
      else {
        insertionSort(array, index + 1,index + 1)
      }
    }
    else
      array

    sortedArray
  }

  val test1 = Array(1)
  println(insertionSort(test1,0,0).mkString(","))

  val test2 = Array(2,1)
  println(insertionSort(test2,0,0).mkString(","))

  val test3 = Array(2,3,4,1,5,1)
  println(insertionSort(test3,0,0).mkString(","))

  val test4 = Array(2,13,1,4,7,6,5,8,9,10,14,11,12,3)
  println(insertionSort(test4,0,0).mkString(","))
}
