package pratice.eopl.impl

import pratice.eopl._
import pratice.eopl.Interpreters._

object MutualRec extends App {
  val interpreter = Interpreters(Const, Var, Diff, IsZero, If, Let, Lambda, Apply, Letrec)
  val pgm = """
    letrec
      (even x) = if zero?(x) then 1 else (odd -(x, 1))
      (odd x) = if zero?(x) then 0 else (even -(x, 1))
    in (odd 13)
            """
  require(interpreter(pgm)(HashEnv()) == NumVal(1))
}

