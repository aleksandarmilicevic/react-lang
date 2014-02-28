package react

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.syntactical._

object Parser extends StandardTokenParsers {

  lexical.delimiters += (",", ".", ":", ";", "(", ")", "[", "]", "{", "}", "=", "<", ">", ">=", "<=", "+", "-", "*", "/", "&&", "||")
  lexical.reserved += ("context", "record", "whenever", "every", "on",
                       "event", "guard", "handler", "action", "invariant",
                       "new", "true", "false", "return", "if", "else", "while", "trigger")

  //TODO

}
