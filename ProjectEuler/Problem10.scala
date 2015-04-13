package coursera.projecteuler

/**
 * Created by Handemelindo on 9/21/2014.
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 * Find the sum of all the primes below two million.
 */
object Problem10 extends App {
  def solve(n: Long): BigInt = {
    val xs = (1l to n).view.map(BigInt(_))
    xs.filter(_.isProbablePrime(5)).sum
  }

  println(solve(2000000))
}
