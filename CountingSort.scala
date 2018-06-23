package Algorithm

/**
  * Created by ken.j.zheng on 6/22/2018.
  */
object CountingSort extends App {
  def getMinimum(array:Array[Int]):Int = {
    var min = array(0)
    array.foreach(x => {
      if(x<min) min = x
    })
    min
  }

  def getMaximum(array:Array[Int]):Int = {
    var max = array(0)
    array.foreach(x => {
      if(x>max) max = x
    })
    max
  }

  def countingSort(input:Array[Int]): Array[Int]={
    //find min & max of array
    val min = getMinimum(input) //input.min
    val max = getMaximum(input) //input.max

    //create index array
    val index = new Array[Int](max-min+1)

    //count each input
    input.foreach(x => {
      index(x-min) += 1
    })

    //sum count
    var previousValue = index(0)
    for(i <- 1 until index.length){
      index(i)=previousValue+index(i)
      previousValue = index(i)
    }

    val position = new Array[Int](input.length)
    //read input from right
    for(i <- input.length-1 to 0 by -1){
      position(index(input(i)-min)-1)=input(i)
      index(input(i)-min) = index(input(i)-min)-1
    }
    position
  }

  val test1 = Array(10,7,12,4,9,13)
  println(countingSort(test1).mkString(","))

  val test2 = Array(9,4,10,8,2,4)
  println(countingSort(test2).mkString(","))

  val test3 = Array(8,2,1,3,10,22,2,3,6,7,9,22,35,66,7,4,1,1,1,3,2,2,2,2,8)
  println(countingSort(test3).mkString(","))
}
