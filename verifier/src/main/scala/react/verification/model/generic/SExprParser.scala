package react.verification.model.generic

import scala.util.parsing.combinator.RegexParsers

abstract class SExpr
case class SAtom(id: String) extends SExpr
case class SApplication(operator: String, arguments: List[SExpr]) extends SExpr
case object SNil extends SExpr

object SExprParser extends RegexParsers {

  protected override val whiteSpace = """(\s|;.*)+""".r

  def nonWhite: Parser[String] = """[^()\s]+""".r 

  def sExpr: Parser[SExpr] = (
      "(" ~ ")" ^^^ SNil
    | "(" ~> (nonWhite ~ rep(sExpr)) <~ ")" ^^ { case op ~ args => SApplication(op, args) }
    | nonWhite                          ^^ { op => SAtom(op) }
  )

  def sExprs: Parser[List[SExpr]] = (
    sExpr ~ sExprs ^^ { case a ~ b => a :: b }
  | whiteSpace ~> sExprs
  | success(Nil)
  )

  def parse(str: String): Option[List[SExpr]] = {
    val result = parseAll(sExprs, str)
    if (result.successful) {
      val cmds = result.get
      Some(cmds)
    } else {
      None
    }
  }

}
