package coursera.projecteuler

/**
 * Created by Handemelindo on 9/18/2014.
 */
object Problem1 extends App {
  def solve(n: Int): Int = {
    val set = (1 to n).toList
    val multiples = set filter (x => x % 5 == 0 || x % 3 == 0)
    multiples reduce (_ + _)
  }

  println(solve(999))
}
