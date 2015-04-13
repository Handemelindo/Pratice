package coursera.projecteuler

/**
 * Created by Handemelindo on 9/28/2014.
 */
object Problem27 extends App {
  def solve(n: Int) = {
    def nats: Stream[Int] = 0 #:: nats.map(_ + 1)
    def numSerialPrime(a: Int, b: Int) = nats.takeWhile(x => BigInt(x * x + x * a + b).isProbablePrime(5)).size
    for {
      a <- (-n to n).toStream
      b <- (-n to n).toStream
    } yield (a, b, numSerialPrime(a, b))
  }

//  println(solve(1000).maxBy(_._3))
  println(-61 * 971)
}
