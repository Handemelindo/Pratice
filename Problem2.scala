package coursera.projecteuler

import scala.annotation.tailrec

/**
 * Created by Handemelindo on 9/18/2014.
 */
@tailrec
object Problem2 extends App{
  def solve(n: Int): Long = {
    def fibonacci(x: Int, y: Int, sum: Long): Long = {
      if (y > n)
        sum
      else {
        val fib = x + y
        if (fib % 2 == 0)
          fibonacci(y, fib, sum + fib)
        else
          fibonacci(y, fib, sum)
      }
    }
    fibonacci(0, 1, 0)
  }

  println(solve(4000000))
}
