package pratice.eopl

abstract class Interpreter {
  private[eopl] var interpreters: Interpreters = null
  def apply: PartialFunction[Expression, (Environment, Store) => ExpVal]
}
