package pratice.eopl.impl

import pratice.eopl._
import pratice.eopl.Interpreters._

object CallByREFERENCE extends App {
  val interpreter = Interpreters(
    Const, Var, Diff, IsZero, If, Let, Lambda,
    Apply, Letrec, Begin, Set)
  interpreter.mutable = Implicit
  interpreter.parameterPassing = CallByReference
  val pgm1 =
    """
    let swap = x => y =>
               let temp = x
               in begin
                    set x = y;
                    set y = temp
                  end
    in let a = 33
       in let b = 44
          in begin
               ((swap a) b);
               -(a, b)
             end
    """
  require(interpreter(pgm1)(HashEnv()) == NumVal(11))
  val pgm2 = """
    let b = 3
    in let p = x => y =>
               begin
                 set x = 4;
                 y
               end
    in ((p b) b)
    """
  require(interpreter(pgm2)(HashEnv()) == NumVal(4))
}

