package pratice.continuation.impl

import pratice.continuation._
import pratice.continuation.Interpreters._

object LET extends App {
  val interpreter = Interpreters(Const, Var, Diff, IsZero, If, Let)
  val pgm = """
    let z = 5
    in let x = 3
      in let y = -(x, 1)
        in let x = 4
          in -(z, -(x, y))
  """
  require(interpreter(pgm) == NumVal(3))
}
