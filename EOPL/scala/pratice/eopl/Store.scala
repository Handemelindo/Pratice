package pratice.eopl

trait Store {
  def apply(vl: ExpVal): Int
  def alloc(vs: ExpVal*): Int
  def apply(loc: Int): ExpVal
  def update(loc: Int, vl: ExpVal): ExpVal
}

case class ArrayStore(var content: Array[ExpVal]) extends Store {
  def apply(vl: ExpVal): Int = {
    content = content :+ vl
    content.length - 1
  }
  def alloc(vs: ExpVal*): Int = {
    val head = content.length
    content = content :+ vs.head
    vs.tail.foreach(v => content = content :+ v)
    head
  }
  def apply(loc: Int): ExpVal = content(loc)
  def update(loc: Int, vl: ExpVal): ExpVal = {
    content(loc) = vl
    vl
  }
  override def toString: String = content.mkString("ArrayStore{", ", ", "}")
}
object ArrayStore {
  def apply(): ArrayStore = new ArrayStore(Array())
}
