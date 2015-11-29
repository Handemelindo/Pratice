package pratice.eopl

import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {
  private val identifier = """[a-zA-Z]\w*""".r
  private val number = """-?\d+""".r
  private val const = number ^^ LiteralNode
  private val variable = identifier ^^ VarExp
  private val isZero: Parser[IsZeroExp] = "zero?" ~> ("(" ~> expression <~ ")") ^^ {
    case exp => IsZeroExp(exp)
  }
  private val ifExp: Parser[IfExp] = ("if" ~> expression) ~ ("then" ~> expression) ~ ("else" ~> expression) ^^ {
    case p ~ tExp ~ fExp => IfExp(p, tExp, fExp)
  }
  private val diff: Parser[DiffExp] = "-" ~> "(" ~> expression ~ ("," ~> expression) <~ ")" ^^ {
    case exp1 ~ exp2 => DiffExp(exp1, exp2)
  }
  private val let: Parser[LetExp] = "let" ~> (identifier <~ "=") ~ expression ~ ("in" ~> expression) ^^ {
    case vr ~ exp ~ body => LetExp(vr, exp, body)
  }

  private val lambda: Parser[LambdaExp] = (identifier <~ "=>") ~ expression ^^ {
    case vr ~ body => LambdaExp(vr, body)
  }

  private val applyExp: Parser[ApplyExp] = "(" ~> expression ~ expression <~ ")" ^^ {
    case rator ~ rand => ApplyExp(rator, rand)
  }

  private val letrecBody: Parser[LetrecExp] =
    "letrec" ~> rep1("(" ~> identifier ~ (identifier <~ ")") ~ ("=" ~> expression)) ~ ("in" ~> expression) ^^ {
      case procDefs ~ body => LetrecExp(procDefs.map {
        case pNames ~ bVar ~ pBody => (pNames, bVar, pBody)
      }, body)
    }

  private val newRef: Parser[NewRefExp] = "newref" ~> "(" ~> expression <~ ")" ^^ NewRefExp

  private val deRef: Parser[DeRefExp] = "deref" ~> "(" ~> expression <~ ")" ^^ DeRefExp

  private val setRef: Parser[SetRefExp] = "setref" ~> "(" ~> expression ~ ("," ~> expression) <~ ")" ^^ {
    case ref ~ exp => SetRefExp(ref, exp)
  }

  private val set: Parser[SetExp] = "set" ~> identifier ~ ("=" ~> expression) ^^ {
    case lhs ~ rhs => SetExp(lhs, rhs)
  }

  private val begin: Parser[BeginExp] = "begin" ~> rep1sep(expression, ";") <~ "end" ^^ BeginExp

  private val mkPair: Parser[MkPairExp] =
    "pair" ~> ("(" ~> expression) ~ ("," ~> expression <~ ")") ^^ {
      case l ~ r => MkPairExp(l, r)
    }

  private val left: Parser[LeftExp] = "left" ~> "(" ~> expression <~ ")" ^^ LeftExp

  private val right: Parser[RightExp] = "right" ~> "(" ~> expression <~ ")" ^^ RightExp

  private val setLeft: Parser[SetLeftExp] =
    "setleft" ~> ("(" ~> expression) ~ ("," ~> expression <~ ")") ^^ {
      case p ~ l => SetLeftExp(p, l)
    }

  private val setRight: Parser[SetRightExp] =
    "setright" ~> ("(" ~> expression) ~ ("," ~> expression <~ ")") ^^ {
      case p ~ r => SetRightExp(p, r)
    }

  private val expression: Parser[Expression] =
    log(let)("let") |
    log(isZero)("zero?") |
    log(diff)("diff") |
    log(ifExp)("if") |
    log(lambda)("lambda") |
    log(applyExp)("apply") |
    log(letrecBody)("letrec") |
    log(newRef)("newref") |
    log(deRef)("deref") |
    log(setRef)("setref") |
    log(begin)("begin") |
    log(set)("set") |
    log(mkPair)("pair") |
    log(left)("left") |
    log(right)("right") |
    log(setLeft)("set left") |
    log(setRight)("set right") |
    log(variable)("var") |
    log(const)("num")
  private val program = expression ^^ Program
  def parse(input: String): Program = parseAll(program, input) match {
    case Success(pgm, _) => pgm
    case err: NoSuccess => throw new Exception(err.msg)
  }
}
