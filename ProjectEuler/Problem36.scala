package coursera.projecteuler

import scala.annotation.tailrec
import scala.runtime.RichInt

/**
 * Created by Handemelindo on 9/29/2014.
 */
object Problem36 extends App {
  def isBinaryPalindrom(i: Int) = {
    val binary = new RichInt(i).toBinaryString
    binary == binary.reverse
  }

  def palidromSeries(i: Int) = {
    val xs = (1 to 9).map(_.toString).toList
    val ys = (0 to 9).map(_.toString).toList
    @tailrec
    def iter(n: Int, side: List[String], result: List[List[String]]): List[List[String]] = {
      if (n > i) result
      else if (n == 1) iter(n + 1, xs, xs :: result)
      else {
        if ((n & 1) == 1) {
          val newItem = side.flatMap(x => for (y <- ys) yield x + y + x.reverse)
          val newSide = side.flatMap(x => for (y <- ys) yield x + y)
          iter(n + 1, newSide, newItem :: result)
        } else {
          val newPalin = side.map(x => x + x.reverse)
          iter(n + 1, side, newPalin :: result)
        }
      }
    }
    iter(1, Nil, Nil).flatten
  }

  val result = palidromSeries(6).map(_.toInt).filter(isBinaryPalindrom).sum
  println(result)
}
