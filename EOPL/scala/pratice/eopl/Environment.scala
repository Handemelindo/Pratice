package pratice.eopl

import scala.collection.Map

trait Environment {
  def apply(vr: String): ExpVal
  def +(vr: String, vl: ExpVal): Environment
  def +(p: (String, ExpVal)): Environment = this.+(p._1, p._2)
  def ++(ps: List[(String, ExpVal)]): Environment
}
case class HashEnv(map: Map[String, ExpVal]) extends Environment {
  def apply(vr: String): ExpVal = map(vr)
  def +(vr: String, vl: ExpVal): Environment = HashEnv(map + (vr -> vl))
  def ++(ps: List[(String, ExpVal)]): Environment = HashEnv(map ++ ps)
}
object HashEnv {
  def apply(): HashEnv = HashEnv(Map())
}
