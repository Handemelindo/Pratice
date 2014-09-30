package coursera.projecteuler

import Utils.IntUtil

import scala.annotation.tailrec

/**
 * Created by Handemelindo on 9/23/2014.
 */
object Problem25 extends App {
  def solve(n: Int) = {
    def test(x: Int) = x.fibonacci.toString.size == n
    def nat: Stream[Int] = 1 #:: (nat map (_ + 1))
    nat find test
  }

  def solve2(n: Int) = {
    @tailrec
    def fib_iter(pre: BigInt, fib: BigInt, counter: Int): Int = {
      if (fib.toString.size == n) counter
      else fib_iter(fib, fib + pre, counter + 1)
    }
    fib_iter(0, 1, 1)
  }
//  println(4782.fibonacci)
  println(solve2(1000))
}
