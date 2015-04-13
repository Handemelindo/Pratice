package coursera.projecteuler

import scala.annotation.tailrec

/**
 * Created by Handemelindo on 9/20/2014.
 */
object Utils {
  implicit class IntUtil(i: Int) {
    def power(n: Int) = Math.pow(i, n).toInt
    def sqrt = Math.sqrt(i.toDouble).toInt
    def factorsNum = {
      def test(s: Int, m: Int): Int =
        if (i % m != 0)
          s
        else if (i / m == m)
          s + 1
        else
          s + 2
      (1 to i.sqrt).foldLeft(0)(test)
    }
    def factors = {
      (1 to i) filter (i % _ == 0)
    }
    def factorPairs = (1 to sqrt).toStream filter (i % _ == 0) map (x => (x, i / x))
    def isNotRepeat = {
      val str = i.toString
      if (str contains '0') false
      else str.distinct.size == str.size
    }
    def isUnusual = {
      def isPandigital(p: (Int, Int)) = {
        if (p._1.isNotRepeat && p._2.isNotRepeat) {
          val allInOne = p._1.toString + p._2.toString + i.toString
          allInOne.sorted == "123456789"
        } else
          false
      }
      if (!i.isNotRepeat) false
      else factorPairs exists isPandigital
    }
    def factorSum = {
      def collect(s: Int, x: Int): Int = {
        if (i % x != 0)
          s
        else if (i / x == x)
          s + x
        else
          s + x + i / x
      }
      (2 to i.sqrt).foldLeft(0)(collect) + 1
    }
    def isAbundant = factorSum > i
    def isPerfect = factorSum == i
    def isDeficient = factorSum < i
    def fibonacci: BigInt = {
      @tailrec
      def fib_iter(a: BigInt, b: BigInt, p: BigInt, q: BigInt, counter: BigInt): BigInt = {
        if (counter == 0) b
        else if ((counter & 1) == 0) fib_iter(a, b, p * p + q * q, q * q + 2 * p * q, counter / 2)
        else fib_iter(b * q + a * q + a * p, b * p + a * q, p, q, counter - 1)
      }
      fib_iter(BigInt(1), BigInt(0), BigInt(0), BigInt(1), BigInt(i))
    }
    def circular = {
      @tailrec
      def iter(n: Int, result: List[String]): List[String] = {
        if (n == 0) result
        else iter(n - 1, (result.head.tail + result.head.head.toString) :: result)
      }
      iter(i.toString.size - 1, List(i.toString)).map(_.toInt)
    }
    def isCircularPrime = i.circular.forall(BigInt(_).isProbablePrime(5))
  }

  @tailrec
  final def repeat(n: Int)(expr: => Unit): Unit = {
    if (n > 0) {
      expr
      repeat(n - 1)(expr)
    }
  }

  @tailrec
  final def repeat(flag: => Boolean)(expr: => Unit): Unit = {
    if (flag) {
      expr
      repeat(flag)(expr)
    }
  }

  def primes(n: Int, k: Int): List[Int] = (1 to n).filter(BigInt(_).isProbablePrime(k)).toList
}
