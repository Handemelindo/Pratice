package coursera.projecteuler

/**
 * Created by Handemelindo on 9/21/2014.
 */
object Problem16 extends App {
  def solve(n: Int) = {
    val exp = BigInt(1) << n
    exp.toString().sliding(1).map(_.toInt).sum
  }

  println(solve(1000))
}
