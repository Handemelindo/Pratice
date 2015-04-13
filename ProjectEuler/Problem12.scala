package coursera.projecteuler

import scala.annotation.tailrec
import Utils.IntUtil

/**
 * Created by Handemelindo on 9/21/2014.
 */
object Problem12 {

  def find(n: Int) = {
    @tailrec
    def triangleLoop(m: Int, tri: Int): Int = {
      if (tri.factorsNum > n)
        tri
      else
        triangleLoop(m + 1, tri + m)
    }
    triangleLoop(2, 1)
  }

  def main(args: Array[String]) {
    println(find(500))
  }
}
