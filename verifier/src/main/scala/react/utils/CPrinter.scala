package react.utils

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object CPrinter {

  /**
   * @param f the expression to print
   * @param access a function that returns a string that returns the value for this variable, e.g., the value is stored in an external array.
   */
  def expr(f: Formula, literal: Double => String, access: Variable => String): String = f match {
    case v @ Variable(name) => access(v)
    case Literal(l: Long) => literal(l.toDouble)
    case Literal(d: Double) => literal(d)

    case And(lst @ _*) => lst.map(expr(_, literal, access)).mkString("( ", " && ", " )")
    case Or(lst @ _*) =>  lst.map(expr(_, literal, access)).mkString("( ", " || ", " )")
    case Not(f) => "!(" + expr(f, literal, access) +")"

    case Eq(a, b) => "(" + expr(a, literal, access) + " == " + expr(b, literal, access) + ")"
    case Leq(a, b) => "(" + expr(a, literal, access) + " <= " + expr(b, literal, access) + ")"
    case Geq(a, b) => "(" + expr(a, literal, access) + " >= " + expr(b, literal, access) + ")"
    case Lt(a, b) => "(" + expr(a, literal, access) + " < " + expr(b, literal, access) + ")"
    case Gt(a, b) => "(" + expr(a, literal, access) + " > " + expr(b, literal, access) + ")"
    
    case Plus(lst @ _*) => lst.map(expr(_, literal, access)).mkString("( ", " + ", " )")
    case Times(lst @ _*) => lst.map(expr(_, literal, access)).mkString("( ", " * ", " )")
    case Minus(a, b) => "(" + expr(a, literal, access) + " - " + expr(b, literal, access) + ")" 
    case Divides(a @ Literal(_), b @ Literal(_)) => "(" + expr(a, literal, access) + " / " + expr(b, literal, access) + ")" 

    //needs include<cmath>
    case Application(DRealDecl.pow, List(a, b)) => "pow(" + expr(a, literal, access) + "," + expr(b, literal, access) + ")"
    case Application(DRealDecl.cos, List(a)) => "cos(" + expr(a, literal, access) + ")"
    case Application(DRealDecl.sin, List(a)) => "sin(" + expr(a, literal, access) + ")"

    case other => Logger.logAndThrow("CPrinter", Error, "not supported: " + other)
  }

}
