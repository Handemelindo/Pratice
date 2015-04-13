package coursera.projecteuler

import scala.annotation.tailrec
import scala.math.BigInt
import scala.util.Random

/**
 * Created by Handemelindo on 9/19/2014.
 */
object Problem3 extends App {
  @tailrec
  def powerMod(x: Int, s: Int): (Int, Int) =
    if (x % 2 == 0)
      powerMod(x / 2, s + 1)
    else
      (s, x)
  @tailrec
  def testAll(x: Int, n: Int, num: Int): Boolean = {
    val y = x * x % n
    if (y != n - 1 || num == 1)
      y != (n - 1)
    else
      testAll(y, n, num - 1)
  }
  def millerRabin(n: Int): Boolean =
    if(n % 2 == 0)
      false
    else {
      val (s, d) = powerMod(n - 1, 0)
      val a = Random.nextInt(n - 4) + 2
      val x = Math.pow(a.toDouble, d.toDouble).toInt % n
      if (x == 1 || x == (n - 1))
        true
      else {
        testAll(x, n, s - 1)
      }
    }
  def millerRabinTest(k: Int)(n: Int): Boolean =
    if (k == 1)
      millerRabin(n)
    else if (millerRabin(n))
      millerRabinTest(k - 1)(n)
    else
      false

  def solve(n: BigInt): BigInt = {
    def loop(x: BigInt, divisor: BigInt): BigInt = {
      val y = x / divisor
      if (n.mod(y) == BigInt(0) && y.isProbablePrime(4))
        y
      else if (y == BigInt(1))
        BigInt(0)
      else
        loop(x, divisor + 1)
      }
    loop(n, BigInt(1))

  }

  println(solve(BigInt("600851475143")))
}
