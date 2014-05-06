package react

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.syntactical._

import react.ast._
import react.utils.{LogCritical, LogError, LogWarning, LogNotice, LogInfo, LogDebug, Logger}

object Parser extends StandardTokenParsers {

  lexical.delimiters += ( ",", ".", ":", ";",
                          "(", ")", "[", "]", "{", "}",
                          "=", "≠", "<", ">", ">=", "<=", "≤", "≥",
                          "+", "-", "*", "/", "%",
                          "&&", "||", "¬", "∧", "∨",
                          "|", "_",
                          "<-", "←",
                          "->", "→")
  lexical.reserved += ("int", "float", "string", "bool",
                       "package", "import", "context", "state",
                       "every", "on",
                       "event", "TimeOut", "invariant",
                       "new", "true", "false", "not", "as",
                       "return", "if", "then", "else", "do", "while", "let", "var")
  
  def path: Parser[Id] = ident ~ opt("." ~> path) ^^ { case id ~ None => Id(id)
                                                       case id ~ Some(p) => p.addPrefix(id) }

  def tpe: Parser[Type] = (
      "int"         ^^^ TInt
    | "float"       ^^^ TFloat
    | "string"      ^^^ TString
    | "bool"        ^^^ TBool
    | path          ^^ (TObj(_))
    )
  
  def literal: Parser[Literal] = positioned(
      "true"                                ^^^ BoolLit(true)
    | "false"                               ^^^ BoolLit(false)
    | numericLit ~ opt("." ~> numericLit)   ^^ { case int ~ None => IntLit(int.toLong)
                                                 case n1 ~ Some(n2) => FloatLit((n1+"."+n2).toDouble) }
    | stringLit                             ^^ (x => StringLit(x) )
    )


  def lhsSuffix: Parser[(LHS => LHS)] = (
      "[" ~> expr ~ ("]" ~> opt(lhsSuffix)) ^^ { case idx ~ suf => ((x: LHS) => suf.getOrElse((x: LHS) => x)(ArrayAccess(x, idx))) }
    |  ("->"|"→") ~> path ~ opt(lhsSuffix)  ^^ { case fld ~ suf => ((x: LHS) => suf.getOrElse((x: LHS) => x)(FieldAccess(x, fld))) }
    )

  //empty is sucess(())
  def lhs: Parser[LHS] = positioned(
      path ~ opt(lhsSuffix)         ^^ { case id ~ suf => suf.getOrElse((x: LHS) => x)(Ident(id)) }
    )


  //lowest priority
  def expr: Parser[Expr] = positioned(
      expr1 ~ (("&&" | "∧") ~> expr)       ^^ { case e1 ~ e2 => Mk.and(e1, e2) }
    | expr1 ~ (("||" | "∨") ~> expr)       ^^ { case e1 ~ e2 => Mk.or(e1, e2) }
    | expr1
    )
  
  def expr1: Parser[Expr] = positioned(
      expr2 ~ ("=" ~> expr1)               ^^ { case e1 ~ e2 => Mk.eq(e1, e2) }
    | expr2 ~ ("≠" ~> expr1)               ^^ { case e1 ~ e2 => Mk.not(Mk.eq(e1, e2)) }
    | expr2 ~ ("<" ~> expr1)               ^^ { case e1 ~ e2 => Mk.lt(e1, e2) }
    | expr2 ~ (">" ~> expr1)               ^^ { case e1 ~ e2 => Mk.lt(e2, e1) }
    | expr2 ~ (("<=" | "≤") ~> expr1)      ^^ { case e1 ~ e2 => Mk.or(Mk.eq(e1, e2), Mk.lt(e1,e2)) }
    | expr2 ~ ((">=" | "≥") ~> expr1)      ^^ { case e1 ~ e2 => Mk.or(Mk.eq(e1, e2), Mk.lt(e2,e1)) }
    | expr2
    )
  
  def expr2: Parser[Expr] = positioned(
      expr3 ~ ("+" ~> expr2)               ^^ { case e1 ~ e2 => Mk.app( Plus, e1, e2) }
    | expr3 ~ ("-" ~> expr2)               ^^ { case e1 ~ e2 => Mk.app(Minus, e1, e2) }
    | expr3
    )

  def expr3: Parser[Expr] = positioned(
      exprBottom ~ ("*" ~> expr3)          ^^ { case e1 ~ e2 => Mk.app(  Times, e1, e2) }
    | exprBottom ~ ("/" ~> expr3)          ^^ { case e1 ~ e2 => Mk.app(Divides, e1, e2) }
    | exprBottom ~ ("%" ~> expr3)          ^^ { case e1 ~ e2 => Mk.app( Modulo, e1, e2) }
    | exprBottom
    )

  //highest priority
  def exprBottom: Parser[Expr] = positioned(
      literal
    | lhs
    | ("not"| "¬") ~> expr                              ^^ ( expr => Mk.not(expr) )
    | "-" ~> expr                                       ^^ ( expr => Mk.app(UMinus, expr) )
    | "(" ~> rep1sep(expr, ",") <~ ")"                  ^^ ( lst => if (lst.length > 1) Mk.app(Tuple, lst:_*) else lst.head )
    | path ~ opt("(" ~> repsep(expr, ",") <~ ")")       ^^ { case id ~ Some(args) => App(Call(id), args)
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
    | lhs  ~ (("<-" | "←") ~> expr)                             ^^ { case id ~ e => Affect(id, e) }
    | let
    | block
    //TODO for loop (syntacic sugar for while), ... ??
    )


  def patternBottom: Parser[Pattern] = positioned(
      literal                                           ^^ ( lit => PLiteral(lit.l).setType(lit.tpe))
    | path ~ ("(" ~> repsep(pattern, ",") <~ ")")       ^^ { case id ~ args => UnApply(id, args) }
    | "_"                                               ^^ ( _ => WildCard() )
    | ident                                             ^^ ( id => Alias(Id(id), WildCard()))
    | "(" ~> pattern <~ ")"
    )
  
  def pattern2: Parser[Pattern] = positioned(
      patternBottom ~ opt(":" ~> tpe)       ^^ { case p ~ Some(t) => p.setType(t) //TODO careful with PLiteral
                                                 case p ~ None => p }
    )

  def pattern1: Parser[Pattern] = positioned(
      pattern2 ~ opt("as" ~> ident)            ^^ { case p ~ Some(id) => Alias(Id(id), p)
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
    | "every" ~> numericLit ~ block                     ^^ { case period ~ body => PeriodicHandler(period.toInt, body) }
    )

  def state: Parser[State] = positioned(
      "state" ~> ident ~ ("{" ~> rep(let)) ~ rep(handler) <~ "}" ^^ { case id ~ decls ~ handlers => new State(Id(id), decls, handlers) }
    )

  def context: Parser[Context] = positioned(
      "context" ~> ident ~ ("{" ~> rep(let)) ~ rep(state) ~ rep(handler) <~ "}"
        ^^ { case id ~ decls ~ states ~ defaults => new Context(Id(id), decls, states, defaults) }
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
