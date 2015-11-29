package pratice.continuation.impl

import pratice.continuation._
import pratice.continuation.Interpreters._

object IMPLICITREF extends App {
  val interpreter = Interpreters (
    Const, Var, Diff, IsZero, If, Let, Lambda, Apply, Letrec, Assign, Begin)
  val pgm1 =
    """
    let x = 0
    in letrec (even) = if zero?(x)
                  then 1
                  else begin
                         set x = -(x, 1);
                         (odd)
                       end
              (odd) = if zero?(x)
                  then 0
                  else begin
                         set x = -(x, 1);
                         (even)
                       end
       in begin set x = 13; (odd) end
    """
  require(interpreter(pgm1) == NumVal(1))
  val pgm2 =
    """
    let count = 0
    in let g = () => begin
                  set count = -(count, -1);
                  count
                end
       in let a = (g)
          in let b = (g)
             in -(a, b)
    """
  require(interpreter(pgm2) == NumVal(-1))
}

