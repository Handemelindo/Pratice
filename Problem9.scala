package coursera.projecteuler

/**
 * Created by Handemelindo on 9/21/2014.
 * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

   a2 + b2 = c2
   For example, 32 + 42 = 9 + 16 = 25 = 52.

   There exists exactly one Pythagorean triplet for which a + b + c = 1000.
   Find the product abc.
 */
object Problem9 extends App {
  implicit class ProductableTriplet(tri: Triplet) {
    def product = tri._1 * tri._2 * tri._3
  }
  type Triplet = (Long, Long, Long)
  def solve(n: Long): List[Triplet] = {
    if (n < 12)
      Nil
    else {
      val s = n / 2
      def testPytha(x: Long, y: Long, z: Long) = x * x == y * y + z * z
      val result = for {
        x <- s / 2 + 1 until s
        t = n - x
        y <- t / 2 + 1 until x
        z = n - y - x
        if testPytha(x, y, z)
      } yield (x, y, z)
      result.toList
    }
  }
//  println(solve(1000).map(_.product))
  (12l to 100l).toList.map(solve).filterNot(_.isEmpty).foreach(println)
}
