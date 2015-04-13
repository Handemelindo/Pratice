package coursera.projecteuler

import scala.annotation.tailrec

/**
 * Created by Handemelindo on 9/21/2014.
 */
object Problem14 {
  @tailrec
  def numCollatz(n: Long, counter: Int): Int = {
    if (n <= 1)
      counter + 1
    else if ((n & 1) == 1)
      numCollatz(3 * n + 1, counter + 1)
    else
      numCollatz(n / 2, counter + 1)
  }

  def solve(n: Int): Int = {
    @tailrec
    def loop(i: Int, max: Int, result: Int): Int =
      if (i > n)
        result
      else {
        val num = numCollatz(i.toLong, 0)
        if (num > max)
          loop(i + 1, num, i)
        else
          loop(i + 1, max, result)
      }
    loop(1, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(solve(1000000))
  }
}
