package coursera.projecteuler

import Utils.repeat

/**
 * Created by Handemelindo on 9/21/2014.
 */
object Problem15 extends App {
  def solve(n: Int) = {
    var column = Array.fill(n + 1)(1l)
    repeat(n) {
      column = column.scanLeft(0l)(_ + _).tail
    }
    column(n)
  }

  println(solve(20))
}
