package coursera.projecteuler

/**
 * Created by Handemelindo on 9/26/2014.
 */


object Problem32 extends App {
  implicit class IntUtil(i: Long) {
    def sqrt = Math.sqrt(i.toDouble).toLong
    def factorPairs = (1l to sqrt).toStream filter (i % _ == 0) map (x => (x, i / x))
    def isNotRepeat = {
      val str = i.toString
      if (str contains '0') false
      else str.distinct.size == str.size
    }
    def isUnusual = {
      if (i % 10000 == 0) println(i)
      def isPandigital(p: (Long, Long)) = {
        if (p._1.isNotRepeat && p._2.isNotRepeat) {
          val allInOne = p._1.toString + p._2.toString + i.toString
          allInOne.sorted == "123456789"
        } else
          false
      }
      if (!i.isNotRepeat) false
      else factorPairs exists isPandigital
    }
  }

  val unusualNumbers = (1l to 98765432l) filter (_.isUnusual)
  println(unusualNumbers.sum)
}
