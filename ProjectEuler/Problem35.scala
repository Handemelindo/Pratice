package coursera.projecteuler

import scala.annotation.tailrec

/**
 * Created by Handemelindo on 9/29/2014.
 */
object Problem35 extends App {
  implicit class IntUtil(i: Int) {
    def circular = {
      @tailrec
      def iter(n: Int, result: List[String]): List[String] = {
        if (n == 0) result
        else iter(n - 1, (result.head.tail + result.head.head.toString) :: result)
      }
      iter(i.toString.size - 1, List(i.toString)).map(_.toInt)
    }
    def isCircularPrime = {
      println(i)
      i.circular.forall(BigInt(_).isProbablePrime(5))
    }
  }
  def solve(n: Int) = {
    (2 to n).count(_.isCircularPrime)
  }

  println(solve(1000000))
}
