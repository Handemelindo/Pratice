package pratice.continuation

import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {
  private val identifier = """[a-zA-Z]\w*""".r
  private val number = """-?\d+""".r
  private val const: Parser[ConstExp] = number ^^ ConstExp
  private val variable: Parser[VarExp] = identifier ^^ VarExp
  private val isZero: Parser[IsZeroExp] = "zero?" ~> ("(" ~> expression <~ ")") ^^ {
    case exp => IsZeroExp(exp)
  }
  private val ifExp: Parser[IfExp] = ("if" ~> expression) ~ ("then" ~> expression) ~ ("else" ~> expression) ^^ {
    case p ~ tExp ~ fExp => IfExp(p, tExp, fExp)
  }
  private val diff: Parser[DiffExp] = "-" ~> "(" ~> expression ~ ("," ~> expression) <~ ")" ^^ {
    case exp1 ~ exp2 => DiffExp(exp1, exp2)
  }
  private val let: Parser[LetExp] =
    "let" ~>
      rep1(
        (identifier <~ "=") ~
          expression) ~
      ("in" ~> expression) ^^ {
    case bindings ~ body => LetExp(bindings.map {
      case vr ~ vl => vr -> vl
    }, body)
  }

  private val lambda: Parser[LambdaExp] =
    ("(" ~> repsep(identifier, ",") <~ ")") ~ ("=>" ~>
      expression) ^^ {
    case vrs ~ body => LambdaExp(vrs, body)
  }

  private val applyExp: Parser[ApplyExp] = "(" ~> expression ~ rep(expression) <~ ")" ^^ {
    case rator ~ rands => ApplyExp(rator, rands)
  }

  private val letrecBody: Parser[LetrecExp] =
    "letrec" ~>
      rep1(
        "(" ~> identifier ~ (rep(identifier) <~ ")") ~
          ("=" ~> expression)) ~
      ("in" ~> expression) ^^ {
      case procDefs ~ body => LetrecExp(procDefs.map {
        case pNames ~ bVars ~ pBody => (pNames, bVars, pBody)
      }, body)
    }

  private val assign: Parser[AssignExp] = "set" ~> identifier ~ ("=" ~> expression) ^^ {
    case lhs ~ rhs => AssignExp(lhs, rhs)
  }

  private val begin: Parser[BeginExp] = "begin" ~> rep1sep(expression, ";") <~ "end" ^^ BeginExp

  private val expression: Parser[Expression] =
    if (Interpreters.debug)
      log(let)("let") |
        log(isZero)("zero?") |
        log(diff)("diff") |
        log(ifExp)("if") |
        log(lambda)("lambda") |
        log(applyExp)("apply") |
        log(letrecBody)("letrec") |
        log(assign)("assign") |
        log(begin)("begin") |
        log(variable)("var") |
        log(const)("num")
    else
      let |
        isZero |
        diff |
        ifExp |
        lambda |
        applyExp |
        letrecBody |
        assign |
        begin |
        variable |
        const
  private val program = expression ^^ Program
  def parse(input: String): Program = parseAll(program, input) match {
    case Success(pgm, _) => pgm
    case err: NoSuccess => throw new Exception(err.msg)
  }
}
