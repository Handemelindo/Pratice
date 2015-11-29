package pratice.continuation.impl

import pratice.continuation._
import pratice.continuation.Interpreters._

object PROC extends App {
  val interpreter = Interpreters(Const, Var, Diff, IsZero, If, Let, Lambda, Apply)
  val pgm = """
    let x = 200
    in let f = (z) => -(z, x)
      in let x = 100
        in let g = (z) => -(z, x)
          in -((f 1), (g 1))
  """
  require(interpreter(pgm) == NumVal(-100))
}
