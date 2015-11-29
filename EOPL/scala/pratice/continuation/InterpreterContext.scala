package pratice.continuation

/**
  * Created by @author YuanhangWang
  * on 2015/11/26.
  * <p>
  */
final case class InterpreterContext(env: Environment, store: Store, k: Continuation, interpreter: Interpreters) {
  def +(vr: String, vl: ExpVal): InterpreterContext = {
    val newEnv = env + (vr, store + vl)
    copy(env = newEnv)
  }

  def >+[A](vr: String, vl: A)(f: A => ExpVal): InterpreterContext = {
    val newEnv = env + (vr, store + f(vl))
    copy(env = newEnv)
  }

  def ++(bindingDefns: List[(String, ExpVal)]): InterpreterContext = {
    val bindings = for ((vr, vl) <- bindingDefns) yield vr -> (store + vl)
    val newEnv = env ++ bindings
    copy(env = newEnv)
  }

  def >++[A](bindingDefns: List[(String, A)])(f: A => ExpVal): InterpreterContext = {
    val bindings = for ((vr, vl) <- bindingDefns) yield vr -> (store + f(vl))
    val newEnv = env ++ bindings
    copy(env = newEnv)
  }
}
object InterpreterContext {
  def init(interpreters: Interpreters): InterpreterContext =
    InterpreterContext(HashEnv(), ArrayStore(), Continuation.endCont, interpreters)
}
