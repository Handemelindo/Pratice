package pratice.continuation.impl

import pratice.continuation._
import pratice.continuation.Interpreters._

object LETREC extends App {
  val interpreter = Interpreters(Const, Var, Diff, IsZero, If, Let, Lambda, Apply, Letrec)
  val pgm = """
    letrec (double x) =
      if zero?(x) then 0 else -((double -(x, 1)), -2)
    in (double 6)
  """
  require(interpreter(pgm) == NumVal(12))
}
