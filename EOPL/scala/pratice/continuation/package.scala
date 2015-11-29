package pratice

/**
  * Created by @author YuanhangWang
  * on 2015/11/26.
  * <p>
  */
package object continuation {
  case class Program(exp: Expression)

  sealed trait Expression
  case class ConstExp(num: String) extends Expression
  case class VarExp(vr: String) extends Expression
  case class DiffExp(exp1: Expression, exp2: Expression) extends Expression
  case class IsZeroExp(exp: Expression) extends Expression
  case class IfExp(p: Expression, tExp: Expression, fExp: Expression) extends Expression
  case class LetExp(bindings: List[(String, Expression)], body: Expression) extends Expression
  case class LambdaExp(vrs: List[String], body: Expression) extends Expression
  case class ApplyExp(rator: Expression, rands: List[Expression]) extends Expression
  case class LetrecExp(procDefs: List[(String, List[String], Expression)], letrecBody: Expression) extends Expression
  case class AssignExp(lhs: String, rhs: Expression) extends Expression
  case class BeginExp(exps: List[Expression]) extends Expression

  sealed trait ExpVal
  case class NumVal(num: Int) extends ExpVal
  case class ProcVal(vrs: List[String], body: Expression, var bindEnv: Environment) extends ExpVal
  class BoolVal private (val bool: Boolean) extends ExpVal {
    override def toString: String = s"BoolVal($bool)"
  }
  object BoolVal {
    private lazy val t = new BoolVal(true)
    private lazy val f = new BoolVal(false)
    def apply(bool: Boolean): BoolVal = if (bool) t else f
    def unapply(boolVal: BoolVal): Option[Boolean] = Some(boolVal.bool)
  }
  case class Thunk(exp: Expression, env: Environment) extends ExpVal

  sealed trait DenVal
  case class RefVal(loc: Int) extends DenVal
}
