package pratice.eopl.impl

import pratice.eopl.Interpreters._
import pratice.eopl._

object CallByNAME extends App {
  val interpreter = Interpreters(
    Const, Var, Diff, IsZero, If, Let, Lambda,
    Apply, Letrec, Begin, Set)
  interpreter.mutable = Implicit
  interpreter.parameterPassing = CallByName
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
  val pgm3 = """
    letrec (infinite x) = (infinite -(x, -1))
    in let f = z => 11
       in let k = 0
           in (f (infinite k))
    """
  require(interpreter(pgm3)(HashEnv()) == NumVal(11))
  val pgm4 = """
    let effect = z => begin set z = -(z, -1); z end
    in let threeTimes = f => -(f, -(f, f))
       in let x = 11
          in (threeTimes (effect x))
    """
  require(interpreter(pgm4)(HashEnv()) == NumVal(13))
}

