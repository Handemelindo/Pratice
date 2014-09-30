package coursera.projecteuler

import scala.io.Source

/**
 * Created by Handemelindo on 9/21/2014.
 */
object Problem8 {
  def read = Source.fromFile("D:/p8.txt").getLines().toList
  lazy val nums = read.map(_.sliding(1).map(_.toLong)).flatten

  def solve(xs: List[Long], n: Int): Long =
    xs.view.sliding(n).map(_.product).reduce((x, y) => if (x > y) x else y)

  println(solve(nums, 13))
}
