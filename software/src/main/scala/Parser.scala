package react

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.syntactical._

import react.utils.{LogCritical, LogError, LogWarning, LogNotice, LogInfo, LogDebug, Logger}

object Parser extends StandardTokenParsers {

  lexical.delimiters += ( ",", ".", ":", ";",
                          "(", ")", "[", "]", "{", "}",
                          "=", "<", ">", ">=", "<=", "≤", "≥",
                          "+", "-", "*", "/", "%",
                          "&&", "||", "¬", "∧", "∨",
                          "|", "_",
                          "<-", "←")
  lexical.reserved += ("package", "import", "context", "state", "initial",
                       "whenever", "every", "on",
                       "event", "TimeOut", "action", "invariant",
                       "new", "true", "false", "not", "as",
                       "return", "if", "then", "else", "do", "while", "send", "let", "var")

  
  def literal: Parser[Literal] = positioned(
      "true"                                        ^^^ Literal(true).setType(TBool)
    | "false"                                       ^^^ Literal(false).setType(TBool)
    | numericLit ~ opt("." ~> numericLit)           ^^ { case int ~ None => Literal(int.toInt).setType(TInt)
                                                         case n1 ~ Some(n2) => Literal((n1+"."+n2).toDouble).setType(TFloat) }
    | stringLit                                     ^^ ( str => Literal(str).setType(TString) )
    )

  def path: Parser[Id] = ident ~ opt("." ~> path) ^^ { case id ~ None => Id(id)
                                                       case id ~ Some(p) => p.addPrefix(id) }

  private def mkAnd(e1: Expr, e2: Expr) = App(And, List(e1, e2))
  private def mkOr(e1: Expr, e2: Expr) = App(Or, List(e1, e2))

  //lowest priority
  def expr: Parser[Expr] = positioned(
      expr1 ~ (("&&" | "∧") ~> expr)       ^^ { case e1 ~ e2 => mkAnd(e1, e2) }
    | expr1 ~ (("||" | "∨") ~> expr)       ^^ { case e1 ~ e2 => mkOr(e1, e2) }
    | expr1
    )

  private def mkLt(e1: Expr, e2: Expr) = App(Lt, List(e1, e2))
  private def mkEq(e1: Expr, e2: Expr) = App(Equal, List(e1, e2))
  
  def expr1: Parser[Expr] = positioned(
      expr2 ~ ("=" ~> expr1)               ^^ { case e1 ~ e2 => mkEq(e1, e2) }
    | expr2 ~ ("<" ~> expr1)               ^^ { case e1 ~ e2 => mkLt(e1, e2) }
    | expr2 ~ (">" ~> expr1)               ^^ { case e1 ~ e2 => mkLt(e2, e1) }
    | expr2 ~ (("<=" | "≤") ~> expr1)      ^^ { case e1 ~ e2 => mkOr(mkEq(e1, e2), mkLt(e1,e2)) }
    | expr2 ~ ((">=" | "≥") ~> expr1)      ^^ { case e1 ~ e2 => mkOr(mkEq(e1, e2), mkLt(e2,e1)) }
    | expr2
    )
  
  def expr2: Parser[Expr] = positioned(
      expr3 ~ ("+" ~> expr2)               ^^ { case e1 ~ e2 => App( Plus, List(e1, e2)) }
    | expr3 ~ ("-" ~> expr2)               ^^ { case e1 ~ e2 => App(Minus, List(e1, e2)) }
    | expr3
    )

  def expr3: Parser[Expr] = positioned(
      exprBottom ~ ("*" ~> expr3)          ^^ { case e1 ~ e2 => App(  Times, List(e1, e2)) }
    | exprBottom ~ ("/" ~> expr3)          ^^ { case e1 ~ e2 => App(Divides, List(e1, e2)) }
    | exprBottom ~ ("%" ~> expr3)          ^^ { case e1 ~ e2 => App( Modulo, List(e1, e2)) }
    | exprBottom
    )

  //highest priority
  def exprBottom: Parser[Expr] = positioned(
      literal
    | path                                              ^^ ( id => Ident(id) )
    | "not" ~> expr                                     ^^ ( expr => App(Not, List(expr)) )
    |   "¬" ~> expr                                     ^^ ( expr => App(Not, List(expr)) )
    | "-" ~> expr                                       ^^ ( expr => App(UMinus, List(expr)) )
    | "(" ~> rep1sep(expr, ",") <~ ")"                  ^^ ( lst => if (lst.length > 1) App(Tuple, lst) else lst.head )
    | path ~ opt("(" ~> repsep(expr, ",") <~ ")")       ^^ { case id ~ Some(args) => App(Call, Ident(id) :: args)
                                                             case id ~ None => Ident(id) }
    | "new" ~> path ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case id ~ args => New(id, args) }
    )

  def block: Parser[Block] = positioned(
      "{" ~> repsep(stmnt, ";") <~ "}"                          ^^ ( stm => Block(stm) )
  )

  def let: Parser[Let] = positioned(
      "let" ~> ident ~ (("<-" | "←") ~> expr)                   ^^ { case id ~ e => Let(Id(id), e, false) }
    | "var" ~> ident ~ (("<-" | "←") ~> expr)                   ^^ { case id ~ e => Let(Id(id), e, true) }
  )

  def stmnt: Parser[Stmnt] = positioned(
      "if" ~> expr ~ ("then" ~> stmnt) ~ opt("else" ~> stmnt)   ^^ { case c ~ t ~ f => ITE(c, t, f.getOrElse(Block(Nil))) }
    | "while" ~> expr ~ ("do" ~> stmnt)                         ^^ { case c ~ b => While(c, b) }
    | "do" ~> stmnt ~ ("while" ~> expr)                         ^^ { case b ~ c => Block(List(b, While(c,b))) }
    | "return" ~> expr                                          ^^ ( e => Return(e) )
    | path ~ (("<-" | "←") ~> expr)                             ^^ { case id ~ e => Affect(id, e) }
    | let
    | block
    //TODO send, for loop (syntacic sugar for while), ... ??
    )


  def patternBottom: Parser[Pattern] = positioned(
      literal                                           ^^ ( lit => PLiteral(lit.l).setType(lit.tpe))
    | path ~ ("(" ~> repsep(pattern, ",") <~ ")")       ^^ { case id ~ args => UnApply(id, args) }
    | "_"                                               ^^ ( _ => WildCard() )
    | ident                                             ^^ ( id => Alias(Id(id), WildCard()))
    | "(" ~> pattern <~ ")"
    )

  def pattern1: Parser[Pattern] = positioned(
      patternBottom ~ opt("as" ~> ident)       ^^ { case p ~ Some(id) => Alias(Id(id), p)
                                                    case p ~ None => p }
    )

  def pattern: Parser[Pattern] = positioned(
      rep1sep(pattern1, "|")                   ^^ ( lst => if (lst.length > 1) lst.head else Alternatives(lst) )
    )

  def moveTo: Parser[Id] = //TODO better
    ident ^^ ( id => Id(id) )

  def handler: Parser[Handler] = positioned(
      "on" ~> "TimeOut" ~> numericLit ~ block ~ moveTo  ^^ { case to ~ body ~ dst => TimeOutHandler(to.toInt, body, dst) }
    | "on" ~> pattern ~ block ~ moveTo                  ^^ { case evt ~ body ~ dst => EventHandler(evt, body, dst) }
    | "whenever" ~> expr ~ block                        ^^ { case cond ~ body => ConditionHandler(cond, body) }
    | "every" ~> numericLit ~ block                     ^^ { case period ~ body => PeriodicHandler(period.toInt, body) }
    )

  def state: Parser[State] = positioned(
      "state" ~> ident ~ ("{" ~> rep(let)) ~ rep(handler) <~ "}" ^^ { case id ~ decls ~ handlers => new State(Id(id), decls, handlers) }
    )

  def context: Parser[Context] = positioned(
      "context" ~> ident ~ ("{" ~> "initial" ~> ident) ~ rep(let) ~ rep(state) ~ rep(handler) <~ "}"
        ^^ { case id ~ init ~ decls ~ states ~ defaults => new Context(Id(id), decls, states, Id(init), defaults) }
    )

  def pckg: Parser[Id] = "package" ~> path

  def imprt: Parser[Id] = "import" ~> path

  def file(fn: String): Parser[CompilationUnit] = 
    opt(pckg) ~ rep(imprt) ~ rep(context) ^^ { case pck ~ imps ~ ctx => new CompilationUnit(fn, pck.getOrElse(Id("_noPackage_")), imps, ctx) }
  

  def apply(fn: String, str: String): Option[CompilationUnit] = {
    val tokens = new lexical.Scanner(str)
    val result = phrase(file(fn))(tokens)
    if (result.successful) {
      Some(result.get)
    } else {
      Logger("Parser", LogError, "parsing error: " + result.toString) //TODO fn
      None
    }
  }

}
