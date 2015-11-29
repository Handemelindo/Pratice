package pratice.continuation

/**
  * Created by @author YuanhangWang
  * on 2015/11/26.
  * <p>
  */
trait Environment {
  def apply(vr: String): DenVal
  def +(vr: String, vl: DenVal): Environment
  def ++(ps: List[(String, DenVal)]): Environment
}

case class HashEnv(map: Map[String, DenVal]) extends Environment {
  def apply(vr: String): DenVal = map(vr)
  def +(vr: String, vl: DenVal): Environment = HashEnv(map + (vr -> vl))
  def ++(ps: List[(String, DenVal)]): Environment = HashEnv(map ++ ps)
}
object HashEnv {
  def apply(): HashEnv = HashEnv(Map())
}

