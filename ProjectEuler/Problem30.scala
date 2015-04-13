package coursera.projecteuler

/**
 * Created by Handemelindo on 9/25/2014.
 */
object Problem30 extends App {
  def power(x: Char) = Math.pow(x.toInt - 48, 5).toInt

  def convert(n: Int) = n.toString.map(power).sum

  def zipConvert(xs: Vector[Int]): Vector[(Int, Int)] = xs map convert zip xs

  def isEqual(p: (Int, Int)) = p._1 == p._2

  val result = zipConvert(2 to 999999 toVector) filter isEqual map (_._1)
  println(result.sum)
}
