package react.ast

object BoolLit {
  def apply(b: Boolean) = Literal(b).setType(TBool)
  def unapply(e: Expr): Option[Boolean] = e match {
    case Literal(true) => Some(true)
    case Literal(false) => Some(false)
    case _ => None
  }
}

object IntLit {
  def apply(i: Long) = Literal(i).setType(TInt)
  def unapply(e: Expr): Option[Long] = e match {
    case Literal(i: Int) => Some(i)
    case Literal(i: Long) => Some(i)
    case _ => None
  }
}

object FloatLit {
  def apply(f: Double) = Literal(f).setType(TFloat)
  def unapply(e: Expr): Option[Double] = e match {
    case Literal(d: Double) => Some(d)
    case Literal(f: Float) => Some(f)
    case _ => None
  }
}

object StringLit {
  def apply(s: String) = Literal(s).setType(TString)
  def unapply(e: Expr): Option[String] = e match {
    case Literal(s: String) => Some(s)
    case _ => None
  }
}

//shorthands
object Mk {
  def app(sym: Symbol, args: Expr*) = App(sym, args.toList)
  def and(e1: Expr, e2: Expr) = app(And, e1, e2)
  def or(e1: Expr, e2: Expr) = app(Or, e1, e2)
  def lt(e1: Expr, e2: Expr) = app(Lt, e1, e2)
  def eq(e1: Expr, e2: Expr) = app(Equal, e1, e2)
  def not(e: Expr) = app(Not, e)
}
