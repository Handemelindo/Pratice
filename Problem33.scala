package coursera.projecteuler

/**
 * Created by Handemelindo on 9/28/2014.
 */
object Problem33 extends App {
  def solve(n: Int) = {
    for {
      numer <- 10 to n
      denom <- (numer + 1) to n
      if test(numer, denom)
    } yield new Rational(numer, denom)
  }

  def test(numer: Int, denom: Int) = {
    val ratThis = new Rational(numer, denom)
    val norNumer = numer.toString diff denom.toString
    val norDenom = denom.toString diff numer.toString
    if (numer % 10 == 0 && denom % 10 == 0) false
    else if (ratThis.factor == 1) false
    else if (norNumer == "" || norDenom == "") false
    else if (norNumer.size == numer.toString.size) false
    else {
      val ratThat = new Rational(norNumer.toInt, norDenom.toInt)
      ratThis == ratThat
    }
  }

  class Rational(n: Int, d: Int) {
    val factor = gcd(n, d)
    val numer = n / factor
    val denom = d / factor

    def gcd(a: Int, b: Int): Int = {
      if (b == 0) a
      else gcd(b, a % b)
    }

    def ==(that: Rational) = this.numer == that.numer && this.denom == that.denom
    def *(that: Rational) = new Rational(this.numer * that.numer, this.denom * that.denom)
    override def toString = s"$numer / $denom"
  }

  println(solve(100).foldLeft(new Rational(1, 1))(_*_))
}
