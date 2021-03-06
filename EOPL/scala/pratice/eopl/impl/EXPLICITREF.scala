package pratice.eopl.impl

import pratice.eopl._
import pratice.eopl.Interpreters._

object EXPLICITREF extends App {
  val interpreter = Interpreters(
    Const, Var, Diff, IsZero, If, Let, Lambda,
    Apply, Letrec, NewRef, SetRef, DeRef, Begin)
  val pgm1 =
    """
    let x = newref(0)
    in letrec (even dummy)
                = if zero?(deref(x))
                  then 1
                  else begin
                         setref(x, -(deref(x), 1));
                         (odd 888)
                       end
              (odd dummy)
                = if zero?(deref(x))
                  then 0
                  else begin
                         setref(x, -(deref(x), 1));
                         (even 888)
                       end
       in begin setref(x, 13); (odd 888) end
    """
  require(interpreter(pgm1)(HashEnv()) == NumVal(1))
  val pgm2 = """
    let g = let counter = newref(0)
            in dummy => begin
                          setref(counter, -(deref(counter), -1));
                          deref(counter)
                        end
    in let a = (g 11)
       in let b = (g 11)
          in -(a, b)
             """
  require(interpreter(pgm2)(HashEnv()) == NumVal(-1))
}
