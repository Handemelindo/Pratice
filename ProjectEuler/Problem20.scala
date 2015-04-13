package coursera.projecteuler

import scala.annotation.tailrec

/**
 * Created by Handemelindo on 9/21/2014.
 */
object Problem20 extends App {
  def solve(n: Int) = {
    @tailrec
    def fact(x: BigInt, prod: BigInt): BigInt =
      if (x > n) prod
      else fact(x + 1, prod * x)
    fact(BigInt(1), BigInt(1)).toString.sliding(1).map(_.toInt).sum
  }

  println(solve(100))
}
