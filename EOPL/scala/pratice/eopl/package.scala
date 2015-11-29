package pratice

package object eopl {
  case class Program(exp: Expression)
  sealed trait Expression
  case class LiteralNode(num: String) extends Expression
  case class VarExp(vr: String) extends Expression
  case class DiffExp(exp1: Expression, exp2: Expression) extends Expression
  case class IsZeroExp(exp: Expression) extends Expression
  case class IfExp(p: Expression, tExp: Expression, fExp: Expression) extends Expression
  case class LetExp(vr: String, exp: Expression, body: Expression) extends Expression
  case class LambdaExp(vr: String, body: Expression) extends Expression
  case class ApplyExp(rator: Expression, rand: Expression) extends Expression
  case class LetrecExp(procDefs: List[(String, String, Expression)], letrecBody: Expression) extends Expression
  case class NewRefExp(vl: Expression) extends Expression
  case class DeRefExp(ref: Expression) extends Expression
  case class SetRefExp(ref: Expression, vl: Expression) extends Expression
  case class BeginExp(exps: List[Expression]) extends Expression
  case class SetExp(lhs: String, rhs: Expression) extends Expression
  case class MkPairExp(left: Expression, right: Expression) extends Expression
  case class LeftExp(pair: Expression) extends Expression
  case class RightExp(pair: Expression) extends Expression
  case class SetLeftExp(pair: Expression, left: Expression) extends Expression
  case class SetRightExp(pair: Expression, right: Expression) extends Expression

  sealed trait ExpVal
  case class NumVal(num: Int) extends ExpVal
  case class BoolVal(bool: Boolean) extends ExpVal
  case class ProcVal(proc: Procedure) extends ExpVal
  case class RefVal(loc: Int) extends ExpVal
  case class Struct(headLoc: Int, length: Int) extends ExpVal
  case class Thunk(expression: Expression, env: Environment) extends ExpVal

  case class Procedure(vr: String, body: Expression, var env: Environment) {
    private[eopl] def setEnv(env: Environment): Unit = this.env = env
  }
}
