package react.verification.model.generic

import scala.util.parsing.combinator.RegexParsers

abstract class SExpr
case class Atom(id: String) extends SExpr
case class Application(operator: String, arguments: List[SExpr]) extends SExpr
case object SNil extends SExpr

object SExprParser extends RegexParsers {

  def nonWhite: Parser[String] = """[^()\s]+""".r ^^ { _.toString }

  def sExpr: Parser[SExpr] = (
      "(" ~ ")" ^^^ SNil
    | "(" ~> (nonWhite ~ rep(sExpr)) <~ ")" ^^ { case op ~ args => Application(op, args) }
    | nonWhite                          ^^ { op => Atom(op) }
  )

}
