package pratice.continuation

/**
  * Created by @author YuanhangWang
  * on 2015/11/26.
  * <p>
  */
case class Interpreters(interpreters: List[Interpreter]) {
  def apply(ctx: InterpreterContext = InterpreterContext.init(this))(pgm: Program): ExpVal = valueOf(ctx)(pgm.exp)
  def apply(input: String): ExpVal = apply()(Parser.parse(input))

  def valueOf(ctx: InterpreterContext)(exp: Expression): ExpVal =
    interpreters.find(_.valueOf(ctx).isDefinedAt(exp)) match {
      case Some(interpreter) =>
        interpreter.valueOf(ctx)(exp)
      case None => throw new Exception(s"Cannot find interpreter for $exp")
    }
}

object Interpreters {
  def debug = true
  def apply(interpreters: Interpreter*): Interpreters = new Interpreters(interpreters.toList)

  object Const extends Interpreter {
    override def valueOf = ctx => {
      case ConstExp(num) =>
        ctx.k(NumVal(num.toInt))
    }
  }

  object Var extends Interpreter {
    override def valueOf = ctx => {
      case VarExp(vr) =>
        import ctx.{env, store, k, interpreter}
        if (debug) println(vr)
        val loc = env(vr)
        store(loc) match {
          case Thunk(exp, bindEnv) =>
            val r = interpreter.valueOf(ctx.copy(env = bindEnv))(exp)
            store(loc) = r
            r
          case _ => k(store(env(vr)))
        }
    }
  }

  object Diff extends Interpreter {
    override def valueOf = ctx => {
      case DiffExp(exp1, exp2) =>
        import ctx.interpreter
        val cont = Continuation.diffCont(ctx, exp2)(ctx.k)
        interpreter.valueOf(ctx.copy(k = cont))(exp1)
    }
  }

  object IsZero extends Interpreter {
    override def valueOf = ctx => {
      case IsZeroExp(exp) =>
        import ctx.interpreter
        interpreter.valueOf(ctx.copy(k = Continuation.isZeroCont(ctx)(ctx.k)))(exp)
    }
  }

  object If extends Interpreter {
    override def valueOf = ctx => {
      case IfExp(p, t, f) =>
        import ctx.interpreter
        interpreter.valueOf(ctx.copy(k = Continuation.ifCont(ctx, t, f)(ctx.k)))(p)
    }
  }

  object Let extends Interpreter {
    override def valueOf = ctx => {
      case LetExp(bindings, body) =>
        import ctx.interpreter
        val thunks = for ((n, v) <- bindings) yield n -> (ctx.store + Thunk(v, ctx.env))
        val newEnv = ctx.env ++ thunks
        interpreter.valueOf(ctx.copy(env = newEnv))(body)
    }
  }

  object Lambda extends Interpreter {
    override def valueOf = ctx => {
      case LambdaExp(vrs, body) =>
        import ctx.{env, k}
        k(ProcVal(vrs, body, env))
    }
  }

  object Apply extends Interpreter {
    override def valueOf = ctx => {
      case ApplyExp(rator, rands) =>
        import ctx.interpreter
        val cont = Continuation.applyCont(ctx, rands)(ctx.k)
        interpreter.valueOf(ctx.copy(k = cont))(rator)
    }
  }

  object Letrec extends Interpreter {
    override def valueOf = ctx => {
      case LetrecExp(procDefns, letrecBody) =>
        import ctx.{env, store, interpreter}
        val procs =
          for ((pName, vars, body) <- procDefns) yield pName -> ProcVal(vars, body, env)
        val denVals =
          for ((pName, proc) <- procs) yield pName -> (store + proc)
        val newEnv = env ++ denVals
        for ((pName, proc) <- procs) proc.bindEnv = newEnv
        interpreter.valueOf(ctx.copy(env = newEnv))(letrecBody)
    }
  }

  object Assign extends Interpreter {
    override def valueOf = ctx => {
      case AssignExp(lhs, rhs) =>
        import ctx.interpreter
        val cont = Continuation.assignCont(ctx, lhs)(ctx.k)
        interpreter.valueOf(ctx.copy(k = cont))(rhs)
    }
  }

  object Begin extends Interpreter {
    override def valueOf = ctx => {
      case BeginExp(instructions) =>
        import ctx.interpreter
        val cont = Continuation.beginCont(ctx, instructions.tail)(ctx.k)
        interpreter.valueOf(ctx.copy(k = cont))(instructions.head)
    }
  }

}
