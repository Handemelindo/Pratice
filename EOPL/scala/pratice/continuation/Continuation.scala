package pratice.continuation

/**
  * Created by @author YuanhangWang
  * on 2015/11/26.
  * <p>
  */
sealed trait Continuation { k =>
  def apply(vl: ExpVal): ExpVal
  def compose(g: Continuation): Continuation = new Continuation {
    override def apply(vl: ExpVal): ExpVal = k(g(vl))
  }
}
object Continuation {
  val endCont: Continuation = new Continuation {
    override def apply(vl: ExpVal): ExpVal = {
      println(s"$vl $this")
      vl
    }
    override def toString: String = "end of continuation"
  }

  def isZeroCont(ctx: InterpreterContext)(k: Continuation): Continuation = new Continuation {
    override def apply(vl: ExpVal): ExpVal = {
      if (Interpreters.debug) println(s"$vl $this")
      val NumVal(num) = vl
      k(BoolVal(num == 0))
    }
    override def toString: String = s"isZero next:\n$k"
  }

  def letCont(ctx: InterpreterContext,
              bindingDefns: List[(String, Expression)],
              vr: String,
              body: Expression)
             (k: Continuation): Continuation = new Continuation {
    override def apply(vl: ExpVal): ExpVal = bindingDefns match {
      case head :: tail =>
        if (Interpreters.debug) {
          println(s"$vl $this")
          println
        }
        import ctx.interpreter
        val (nVr, nVl) = bindingDefns.head
        val cont = Continuation.letCont(ctx + (vr, vl), bindingDefns.tail, nVr, body)(k)
        interpreter.valueOf(ctx.copy(k = cont))(nVl)
      case Nil =>
        if (Interpreters.debug) {
          println(s"$vl $this")
          println
        }
        ctx.interpreter.valueOf(ctx + (vr, vl))(body)
    }
    override def toString: String = s"let ( $vr = _; $bindingDefns) in $body next:\n$k"
  }

  def diffCont(ctx: InterpreterContext, exp2: Expression)(k: Continuation): Continuation = new Continuation {
    override def apply(vl1: ExpVal): ExpVal = {
      import ctx.interpreter
      if (Interpreters.debug) {
        println(s"$vl1 $this")
        println
      }
      def cont: Continuation = new Continuation {
        override def apply(vl2: ExpVal): ExpVal = {
          if (Interpreters.debug) {
            println(s"$vl2 $this")
            println
          }
          val (NumVal(num1), NumVal(num2)) = (vl1, vl2)
          k(NumVal(num1 - num2))
        }
        override def toString: String = s"diff($vl1, $exp2) next:\n$k"
      }

      interpreter.valueOf(ctx.copy(k = cont))(exp2)
    }
    override def toString: String = s"diff(_, $exp2) next:\n$k"
  }

  def ifCont(ctx: InterpreterContext,
             t: Expression,
             f: Expression)
            (k: Continuation): Continuation = new Continuation {
    override def apply(p: ExpVal): ExpVal = {
      import ctx.interpreter
      if (Interpreters.debug) {
        println(s"$p $this")
        println
      }
      val BoolVal(bool) = p
      if (bool) interpreter.valueOf(ctx)(t)
      else interpreter.valueOf(ctx)(f)
    }
    override def toString: String = s"if(_) then $t else $f next:\n$k"
  }

  def applyCont(ctx: InterpreterContext,
                rands: List[Expression])
               (k: Continuation): Continuation = new Continuation {
    override def apply(rator: ExpVal): ExpVal = {
      import ctx.interpreter
      if (Interpreters.debug) {
        println(s"$rator $this")
        println
      }
      val ProcVal(vrs, body, bindEnv) = rator
      require(vrs.size == rands.size)
      val newEnv = bindEnv ++ vrs.zip(rands.map(ctx.store + Thunk(_, ctx.env)))
      interpreter.valueOf(ctx.copy(env = newEnv))(body)
    }
    override def toString: String = s"evaluate rator next:\n$k"
  }

  def assignCont(ctx: InterpreterContext, lhs: String)(k: Continuation): Continuation = new Continuation {
    override def apply(vl: ExpVal): ExpVal = {
      import ctx.{env, store}
      if (Interpreters.debug) {
        println(s"$vl $this")
        println
      }
      store(env(lhs)) = vl
      k(vl)
    }
    override def toString: String = s"assign $lhs = _ next:\n$k"
  }

  def beginCont(ctx: InterpreterContext, instructions: List[Expression])(k: Continuation): Continuation = new Continuation {
    override def apply(vl: ExpVal): ExpVal = instructions match {
      case head :: tail =>
        import ctx.interpreter
        if (Interpreters.debug) {
          println(s"$vl $this")
          println
        }
        interpreter.valueOf(ctx.copy(k = beginCont(ctx, tail)(k)))(head)
      case Nil =>
        if (Interpreters.debug) {
          println(s"$vl $this")
          println
        }
        k(vl)
    }
    override def toString: String = s"begin $instructions next:\n$k"
  }
}
