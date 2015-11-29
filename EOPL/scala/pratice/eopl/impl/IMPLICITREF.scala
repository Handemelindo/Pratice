package pratice.eopl.impl

import pratice.eopl._
import pratice.eopl.Interpreters._

object IMPLICITREF extends App {
  val interpreter = Interpreters(
    Const, Var, Diff, IsZero, If, Let, Lambda,
    Apply, Letrec, Set, Begin)
  interpreter.mutable = Implicit
  val pgm1 =
    """
    let x = 0
    in letrec (even dummy)
                = if zero?(x)
                  then 1
                  else begin
                         set x = -(x, 1);
                         (odd 888)
                       end
              (odd dummy)
                = if zero?(x)
                  then 0
                  else begin
                         set x = -(x, 1);
                         (even 888)
                       end
       in begin set x = 13; (odd 888) end
    """
  val pgm2 =
    """
    let g = let count = 0
            in dummy => begin
                          set count = -(count, -1);
                          count
                        end
    in let a = (g 11)
      in let b = (g 11)
        in -(a, b)
    """
  require(interpreter(pgm1)(HashEnv()) == NumVal(1))
  require(interpreter(pgm2)(HashEnv()) == NumVal(-1))
}

