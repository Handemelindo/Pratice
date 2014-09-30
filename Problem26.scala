package coursera.projecteuler

import scala.annotation.tailrec

/**
 * Created by Handemelindo on 9/23/2014.
 */
object Problem26 extends App {
  type Series = Array[Int]
  def solve(d: Int) = {
    def calDigits(x: Int) = {
      @tailrec
      def calDigits_iter(x: Int, dividend: Int, digits: Series, remainders: Series): (Int, Series) = {
        if (dividend < x)
          calDigits_iter(x, dividend * 10, digits :+ 0, remainders :+ (-1))
        else {
          val remainder = dividend % x
          val index = remainders indexOf remainder
          val newDigits = digits :+ (dividend / x)
          // we found the reciprocal cycle.
          if (remainder == 0)
            (0, newDigits)
          else if (index != -1)
            (remainders.size - index, newDigits)
          else
            calDigits_iter(x, remainder * 10, newDigits, remainders :+ remainder)
        }
      }
      calDigits_iter(x, 10, Array(), Array(1))
    }

    def formatCycle(digits: Series, from: Int) = {
      val (normal, cycle) = digits.splitAt(from)
      "0." + normal.mkString + "(" + cycle.mkString + ")"
    }

    (1 to d).map(calDigits).zipWithIndex.maxBy(_._1._1)
  }

  def show(p: ((Int, Series), Int)) = {
    p._2 + "--->" + p._1._1 + ":" + p._1._2.mkString
  }

//  println(solve(10).map(_.mkString).mkString("\n"))
  println(show(solve(1000)))
}
