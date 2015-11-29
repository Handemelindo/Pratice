package pratice.eopl.impl

import pratice.eopl._
import pratice.eopl.Interpreters._

object LETREC extends App {
  val interpreter = Interpreters(Const, Var, Diff, IsZero, If, Let, Lambda, Apply, Letrec)
  val pgm = """
    letrec (double x) =
      if zero?(x) then 0 else -((double -(x, 1)), -2)
    in (double 6)
  """
  require(interpreter(pgm)(HashEnv()) == NumVal(12))
}
