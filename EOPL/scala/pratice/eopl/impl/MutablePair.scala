package pratice.eopl.impl

import pratice.eopl._
import pratice.eopl.Interpreters._

object MutablePair extends App {
  val interpreter = Interpreters(
    Const, Var, Diff, IsZero, If, Let, Lambda,
    Apply, Letrec, Set, Begin, MkPair,
    Left, Right, SetLeft, SetRight)
  interpreter.mutable = Implicit
  val pgm =
    """
    let glo = pair(11, 22)
    in let f = loc =>
                 let d1 = setright(loc, left(loc))
                 in let d2 = setleft(glo, 99)
                    in -(left(loc), right(loc))
       in (f glo)
    """
  require(interpreter(pgm)(HashEnv()) == NumVal(88))
}

