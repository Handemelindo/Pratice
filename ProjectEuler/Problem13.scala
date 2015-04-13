package coursera.projecteuler

import scala.io.Source

/**
 * Created by Handemelindo on 9/21/2014.
 */
object Problem13 {
  def read = Source.fromFile("D:/p13.txt").getLines().toList
  lazy val nums = read.map(BigInt(_))

  def main(args: Array[String]): Unit = {
    println(nums.sum.toString take 10)
  }
}
