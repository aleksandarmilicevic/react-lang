package react.rewriting

import react.verification.Playground

trait Fields {
  self: ExplorableMacros =>
  import c.universe._

  /** check if a symbol is transient:
   *  transient annotation might be a good idea to minimize the verification state space.
   */
  protected def isTransient(m: TermSymbol) = {
    // http://stackoverflow.com/questions/17236066/scala-macros-checking-for-a-certain-annotation
    m.annotations.exists( _.tree.tpe =:= typeOf[scala.transient] )
  }
  
  protected def isIgnored(m: TermSymbol) = {
    // http://stackoverflow.com/questions/17236066/scala-macros-checking-for-a-certain-annotation
    m.annotations.exists( _.tree.tpe =:= typeOf[react.verification.ignore] )
  }

  /** collect every declared variables (mutable field) */
  protected def collectFields(t: Type): List[TermSymbol] = {
    // http://docs.scala-lang.org/overviews/reflection/symbols-trees-types.html
    // http://stackoverflow.com/questions/17223213/scala-macros-making-a-map-out-of-fields-of-a-class-in-scala
    // http://meta.plasm.us/posts/2013/08/30/horrible-code/
    val flds = t.members.collect{
      //case m: TermSymbol if m.isVar && !m.getter.isPrivate && !isIgnored(m) => m
      case m: TermSymbol if m.isSetter && m.isPublic && !isIgnored(m.accessed.asInstanceOf[TermSymbol]) =>
        m.accessed.asInstanceOf[TermSymbol]
    }.toList
    //Console.err.println("members of " + t + "\n  " + t.members.mkString("\n  "))
    flds
  }
  
  protected def fieldGetter(m: TermSymbol) = {
    //val robot = Select(c.prefix.tree, TermName("robot"))
    //c.Expr(Select(robot, m.getter.name))
    Select(Ident(TermName("robot")), m.getter.name)
  }
  
  protected def fieldSetter(m: TermSymbol) = {
    //val robot = Select(c.prefix.tree, TermName("robot"))
    //c.Expr(Select(robot, m.setter.name))
    Select(Ident(TermName("robot")), m.setter.name)
  }

  protected def supportedFields[T: c.WeakTypeTag] = collectFields(weakTypeOf[T]).filter(isSupported)
  protected def unsupportedFields[T: c.WeakTypeTag] = collectFields(weakTypeOf[T]).filter(!isSupported(_))
  protected def permanentFields[T: c.WeakTypeTag] = supportedFields[T].filter(!isTransient(_))
  protected def transientFields[T: c.WeakTypeTag] = supportedFields[T].filter( isTransient)
  
}
