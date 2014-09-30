package coursera.projecteuler

import Utils.IntUtil

/**
 * Created by Handemelindo on 9/21/2014.
 */
object Problem23 extends App {
  val upperLimit = 28123

  def solve() = {
    val abundantNumbers = (1 to upperLimit).filter(_.isAbundant)
    def abunSumTest(n: Int): Boolean = {
      val xs = abundantNumbers.takeWhile(_ < n).toSet
      xs.exists(x => xs contains (n - x))
    }
    (1 to upperLimit).filterNot(abunSumTest).sum
  }

  println(solve())
}
