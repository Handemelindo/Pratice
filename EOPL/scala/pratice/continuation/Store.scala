package pratice.continuation

/**
  * Created by @author YuanhangWang
  * on 2015/11/26.
  * <p>
  */
trait Store {
  def +(vl: ExpVal): DenVal
  def ++(vl: ExpVal, vs: ExpVal*): DenVal
  def apply(denVal: DenVal): ExpVal
  def update(denVal: DenVal, vl: ExpVal): ExpVal
}

case class ArrayStore(var content: Array[ExpVal]) extends Store {
  def +(vl: ExpVal): DenVal = {
    content = content :+ vl
    RefVal(content.length - 1)
  }
  def ++(vl: ExpVal, vs: ExpVal*): DenVal = {
    val head = content.length
    content = content :+ vl
    vs.foreach(v => content = content :+ v)
    RefVal(head)
  }
  def apply(denVal: DenVal): ExpVal = denVal match {
    case RefVal(loc) => content(loc)
  }
  def update(denVal: DenVal, vl: ExpVal): ExpVal = denVal match {
    case RefVal(loc) =>
    content(loc) = vl
    vl
  }
  override def toString: String = content.mkString("ArrayStore{", ", ", "}")
}
object ArrayStore {
  def apply(): ArrayStore = new ArrayStore(Array())
}

