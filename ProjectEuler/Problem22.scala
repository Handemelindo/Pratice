package coursera.projecteuler

import scala.io.Source

/**
 * Created by Handemelindo on 9/21/2014.
 */
object Problem22 extends App {
  def read = Source.fromFile("D:/p22.txt").getLines().next()
  lazy val names = read.replace("\"", "").split(",")

  def solve() = {
    def char2Int(c: Char): Int = c.toInt - 64
    def name2Score(name: String): Long = name.toCharArray.map(char2Int).sum.toLong
    def calScore(p: (String, Int)): BigInt = BigInt(name2Score(p._1) * (p._2 + 1))

    val scores = names.sorted.zipWithIndex map calScore
    scores.sum
  }

  println(solve())
}
