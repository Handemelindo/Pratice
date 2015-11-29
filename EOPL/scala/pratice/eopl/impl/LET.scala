package pratice.eopl.impl

import pratice.eopl._
import pratice.eopl.Interpreters._

object LET extends App {
  val interpreter = Interpreters(Const, Var, Diff, IsZero, If, Let)
  val pgm = """
    let z = 5
    in let x = 3
      in let y = -(x, 1)
        in let x = 4
          in -(z, -(x, y))
  """
  require(interpreter(pgm)(HashEnv()) == NumVal(3))
}
