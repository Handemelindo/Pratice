package pratice.continuation

/**
  * Created by @author YuanhangWang
  * on 2015/11/26.
  * <p>
  */
trait Interpreter {
  def valueOf: InterpreterContext => PartialFunction[Expression, ExpVal]
}
