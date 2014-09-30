package coursera.projecteuler

import scala.annotation.tailrec
import scala.io.Source

/**
 * Created by Handemelindo on 9/21/2014.
 */
object Problem11 extends App{
  type Matrix = List[List[Long]]
  def read = Source.fromFile("D:/p11.txt").getLines().toList
  lazy val nums = read.map(_.split("\\s+").toList.map(_.toLong))

  def solve(xs: Matrix, n: Int) = {
    def vectorZip(ys: List[Long], zs: List[Long]) =
      ys zip zs map (p => p._1 * p._2)
    def diagZip(list: Matrix) = {
      list.zipWithIndex.map(p => p._1 drop p._2).reduce(vectorZip)
    }
    def solveRight =
      xs.map(_.sliding(n).map(_.product)).flatten
    def solveDown =
      xs.sliding(n).map(_ reduce vectorZip).flatten
    @tailrec
    def diagSliding(m: Matrix, result: Matrix): Matrix =
      if (m.size < n)
        result
      else
        diagSliding(m.tail, diagZip(m take n) :: result)
    def solveRightDown = {
      diagSliding(xs, List()).flatten
    }
    def solveLeftDown = {
      diagSliding(xs.map(_.reverse), List()).flatten
    }
    List(solveRight, solveDown, solveRightDown, solveLeftDown).flatten.max
  }

  val result = solve(nums, 4)
  println(result)
//  println(result.map(_.mkString(" ")).mkString("\n"))
}
