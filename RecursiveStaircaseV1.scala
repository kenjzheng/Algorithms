package Algorithm
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._
/**
  * Created by ken.J.Zheng on 7/12/2018.
  */
//a naive algorithm
object RecursiveStaircaseV1 extends App{
  var count = 0
  def climbStaircase(stairs: Int, steps: Int, buffer: ListBuffer[Int]): Unit = {
    breakable {
      for (i <- 1 to steps) {
        if (stairs - i > 0) {
          buffer.append(i)
          climbStaircase(stairs - i, steps, buffer)
          buffer.remove(buffer.length-1)
        }
        else if(stairs - i == 0){
          buffer.append(i)
          count += 1
          println(count + "->"+ buffer.mkString(","))
          buffer.remove(buffer.length-1)
        }
        else break()
      }
    }
  }

  count = 0
  val test1 = climbStaircase(4,2,ListBuffer[Int]())
  count = 0
  val test2 = climbStaircase(10,5,ListBuffer[Int]())
}
