package coursera.projecteuler

import Utils.repeat

/**
 * Created by Handemelindo on 9/21/2014.
 */
object Problem21 extends App {
  def solve(n: Int) = {
    val amicable = Array.fill(n + 1)(1)
    var step = 1
    var index = 1

    repeat(step <= n) {
      step += 1
      index = 2 * step
      repeat(index <= n) {
        amicable.update(index, amicable(index) + step)
        index += step
      }
    }

    def testAmicable(p: (Int, Int)) = {
      val (x, y) = p
      if (x >= n)
        false
      else
        y == amicable(x) && x > y
    }

    amicable.zipWithIndex.tail filter testAmicable
  }

  println(solve(10000).map(p => p._1 + p._2).sum)
}
