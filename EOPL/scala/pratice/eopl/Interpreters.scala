package pratice.eopl

class Interpreters private (interpreters: List[Interpreter]) {
  import Interpreters._
  def apply(input: String)(env: Environment, store: Store = ArrayStore()): ExpVal = {
    valueOfProgram(Parser.parse(input))(env, store)
  }
  private def valueOfProgram(pgm: Program)(env: Environment, store: Store): ExpVal = {
    val Program(exp1) = pgm
    apply(exp1)(env, store)
  }
  private[eopl] def apply(exp: Expression)(env: Environment, store: Store): ExpVal =
    interpreters.find(_.apply.isDefinedAt(exp)) match {
      case Some(i) => i.apply(exp)(env, store)
      case None => throw new Exception(s"cannot interpret $exp")
    }
  private var _mutable: Mutable = Explicit
  private var _parameterPassing: ParameterPassing = CallByValue
  def mutable_=(mutable: Mutable): Unit =
    _mutable = mutable
  def mutable: Mutable = _mutable
  def parameterPassing_=(parameterPassing: ParameterPassing): Unit =
    _parameterPassing = parameterPassing
  def parameterPassing: ParameterPassing = _parameterPassing
}

object Interpreters {
  sealed trait Mutable
  case object Implicit extends Mutable
  case object Explicit extends Mutable
  sealed trait ParameterPassing
  case object CallByValue extends ParameterPassing
  case object CallByReference extends ParameterPassing
  case object CallByNeed extends ParameterPassing
  case object CallByName extends ParameterPassing
  def apply(interpreters: Interpreter*): Interpreters = {
    val result = new Interpreters(interpreters.toList)
    interpreters.foreach (interpreter => interpreter.interpreters = result)
    result
  }

  object Const extends Interpreter {
    def apply = {
      case LiteralNode(num) => (env, store) => NumVal(num.toInt)
    }
  }

  object Var extends Interpreter {
    def apply = (interpreters.mutable, interpreters.parameterPassing) match {
      case (Explicit, _) => {
        case VarExp(vr) => (env, store) =>
          println(s"var: $vr")
          env(vr)
      }
      case (Implicit, CallByNeed) => {
        case VarExp(vr) => (env, store) => env(vr)
          val RefVal(loc) = env(vr)
          store(loc) match {
            case Thunk(exp, bindEnv) =>
              println(s"thunk var: $vr")
              val result = interpreters(exp)(bindEnv, store)
              store(loc) = result
              result
            case v: ExpVal =>
              println(s"evaluated var: $vr")
              v
          }
      }
      case (Implicit, CallByName) => {
        case VarExp(vr) => (env, store) => env(vr)
          val RefVal(loc) = env(vr)
          store(loc) match {
            case Thunk(exp, bindEnv) =>
              println(s"thunk var: $vr")
              println(store)
              interpreters(exp)(bindEnv, store)
            case v: ExpVal =>
              println(s"evaluated var: $vr")
              v
          }
      }
      case (Implicit, _) => {
        case VarExp(vr) => (env, store) =>
          println(s"var: $vr")
          val RefVal(loc) = env(vr)
          store(loc)
      }
    }
  }

  object Diff extends Interpreter {
    def apply = {
      case DiffExp(exp1, exp2) => (env, store) =>
        val (NumVal(num1), NumVal(num2)) =
          interpreters(exp1)(env, store) -> interpreters(exp2)(env, store)
        println(s"diff($num1, $num2)")
        NumVal(num1 - num2)
    }
  }

  object IsZero extends Interpreter {
    def apply = {
      case IsZeroExp(exp) => (env, store) =>
        val NumVal(num) = interpreters(exp)(env, store)
        println(s"isZero($num)")
        BoolVal(num == 0)
    }
  }

  object If extends Interpreter {
    def apply = {
      case IfExp(p, tExp, fExp) => (env, store) => interpreters(p)(env, store) match {
        case BoolVal(true) =>
          println("if true")
          interpreters(tExp)(env, store)
        case BoolVal(false) =>
          println("if false")
          interpreters(fExp)(env, store)
      }
    }
  }

  object Let extends Interpreter {
    def apply = interpreters.mutable match {
      case Explicit => {
        case LetExp(vr, exp, bd) => (env, store) =>
          val vl = interpreters(exp)(env, store)
          println(s"let $vr = $vl")
          interpreters(bd)(env + (vr -> vl), store)
      }
      case Implicit => {
        case LetExp(vr, exp, bd) => (env, store) =>
          val vl = interpreters(exp)(env, store)
          println(s"let $vr = $vl")
          val loc = store(vl)
          val newEnv = env + (vr -> RefVal(loc))
          interpreters(bd)(newEnv, store)
      }
    }
  }

  object Lambda extends Interpreter {
    def apply = {
      case LambdaExp(vr, body) => (env, store) =>
        println(s"lambda $vr => ...")
        ProcVal(Procedure(vr, body, env))
    }
  }

  object Apply extends Interpreter {
    def apply = (interpreters.mutable, interpreters.parameterPassing) match {
      case (Explicit, _) => {
        case ApplyExp(rator, rand) => (env, store) =>
          val ProcVal(Procedure(vr, body, bindEnv)) = interpreters(rator)(env, store)
          val vl = interpreters(rand)(env, store)
          val newEnv = bindEnv + (vr -> vl)
          println(s"apply $vr as $vl")
          interpreters(body)(newEnv, store)
      }
      case (Implicit, CallByValue) => {
        case ApplyExp(rator, rand) => (env, store) =>
          val ProcVal(Procedure(vr, body, bindEnv)) = interpreters(rator)(env, store)
          val vl = interpreters(rand)(env, store)
          val loc = store(vl)
          val newEnv = bindEnv + (vr -> RefVal(loc))
          println(s"apply $vr as $vl")
          interpreters(body)(newEnv, store)
      }
      case (Implicit, CallByReference) => {
        case ApplyExp(rator, rand) => (env, store) =>
          val ProcVal(Procedure(vr, body, bindEnv)) = interpreters(rator)(env, store)
          rand match {
            case VarExp(id) =>
              println(s"apply $vr as $rand")
              val newEnv = bindEnv + (vr -> env(id))
              interpreters(body)(newEnv, store)
            case _ =>
              println(s"apply $vr as $rand")
              val vl = interpreters(rand)(env, store)
              val loc = store(vl)
              val newEnv = bindEnv + (vr -> RefVal(loc))
              interpreters(body)(newEnv, store)
          }
      }
      case (Implicit, _) => {
        case ApplyExp(rator, rand) => (env, store) =>
          val ProcVal(Procedure(vr, body, bindEnv)) = interpreters(rator)(env, store)
          rand match {
            case VarExp(id) =>
              println(s"apply $vr as $rand")
              val newEnv = bindEnv + (vr -> env(id))
              interpreters(body)(newEnv, store)
            case _ =>
              val thunk = Thunk(rand, env)
              val loc = store(thunk)
              println(s"apply $vr as $thunk in $loc")
              val newEnv = bindEnv + (vr -> RefVal(loc))
              interpreters(body)(newEnv, store)
          }
      }
    }
  }

  object Letrec extends Interpreter {
    def apply = interpreters.mutable match {
      case Explicit => {
        case LetrecExp(procDefs, letrecBody) => (env, store) =>
          val procs = procDefs.map(defi => defi._1 -> Procedure(defi._2, defi._3, env))
          val newEnv = env ++ procs.map { case (n, p) => n -> ProcVal(p) }
          procs.foreach(_._2.setEnv(newEnv))
          interpreters(letrecBody)(newEnv, store)
      }
      case Implicit => {
        case LetrecExp(procDefs, letrecBody) => (env, store) =>
          val procs = procDefs.map(defi => defi._1 -> Procedure(defi._2, defi._3, env))
          val newEnv = env ++ procs.map { case (n, p) => n -> RefVal(store(ProcVal(p))) }
          procs.foreach(_._2.setEnv(newEnv))
          interpreters(letrecBody)(newEnv, store)
      }
    }
  }

  object Set extends Interpreter {
    def apply = {
      case SetExp(lhs, rhs) => (env, store) =>
        val RefVal(loc) = env(lhs)
        val vl = interpreters(rhs)(env, store)
        store(loc) = vl
        vl
    }
  }

  object NewRef extends Interpreter {
    def apply = {
      case NewRefExp(exp) => (env, store) =>
        val vl = interpreters(exp)(env, store)
        val loc = store(vl)
        RefVal(loc)
    }
  }

  object DeRef extends Interpreter {
    def apply = {
      case DeRefExp(exp) => (env, store) =>
        val RefVal(loc) = interpreters(exp)(env, store)
        store(loc)
    }
  }

  object SetRef extends Interpreter {
    def apply = {
      case SetRefExp(refExp, exp) => (env, store) =>
        val RefVal(loc) = interpreters(refExp)(env, store)
        val vl = interpreters(exp)(env, store)
        store(loc) = vl
    }
  }

  object MkPair extends Interpreter {
    def apply = {
      case MkPairExp(l, r) => (env, store) =>
        val lvl = interpreters(l)(env, store)
        val rvl = interpreters(r)(env, store)
        val head = store.alloc(lvl, rvl)
        Struct(head, 2)
    }
  }

  object Left extends Interpreter {
    def apply = {
      case LeftExp(p) => (env, store) =>
        val Struct(head, _) = interpreters(p)(env, store)
        store(head)
    }
  }

  object Right extends Interpreter {
    def apply = {
      case RightExp(p) => (env, store) =>
        val Struct(head, _) = interpreters(p)(env, store)
        store(head + 1)
    }
  }

  object SetLeft extends Interpreter {
    def apply = {
      case SetLeftExp(p, l) => (env, store) =>
        val struct@Struct(head, _) = interpreters(p)(env, store)
        val vl = interpreters(l)(env, store)
        store(head) = vl
        struct
    }
  }

  object SetRight extends Interpreter {
    def apply = {
      case SetRightExp(p, r) => (env, store) =>
        val struct@Struct(head, _) = interpreters(p)(env, store)
        val vl = interpreters(r)(env, store)
        store(head + 1) = vl
        struct
    }
  }

  object Begin extends Interpreter {
    def apply = {
      case BeginExp(exps) => (env, store) =>
        var last: ExpVal = null
        exps.foreach(exp => last = interpreters(exp)(env, store))
        last
    }
  }
}
