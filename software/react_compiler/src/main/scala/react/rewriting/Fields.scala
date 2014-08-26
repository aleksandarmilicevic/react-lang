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
    //m.accessed.annotations.exists( _.tpe =:= typeOf[scala.transient] )
    m.annotations.exists( _.tree.tpe =:= typeOf[scala.transient] )
  }

  /** collect every declared variables (mutable field) */
  protected def collectFields(t: Type): List[TermSymbol] = {
    // http://docs.scala-lang.org/overviews/reflection/symbols-trees-types.html
    // http://stackoverflow.com/questions/17223213/scala-macros-making-a-map-out-of-fields-of-a-class-in-scala
    // http://meta.plasm.us/posts/2013/08/30/horrible-code/
    val flds = t.members.collect{
      case m: TermSymbol if m.isVar && !m.isPrivate => m
    }.toList
    //Console.err.println("collectFields:")
    //for (f <- flds) Console.err.println("  " + f)
    flds
  }
  
  protected def fieldGetter(m: TermSymbol) = {
    val robot = Select(c.prefix.tree, TermName("robot"))
    //c.Expr(Select(robot, m.getter.name))
    Select(robot, m.getter.name)
  }
  
  protected def fieldSetter(m: TermSymbol) = {
    val robot = Select(c.prefix.tree, TermName("robot"))
    //c.Expr(Select(robot, m.setter.name))
    Select(robot, m.setter.name)
  }

  protected def supportedFields[T: c.WeakTypeTag] = collectFields(weakTypeOf[T]).filter(isSupported)
  protected def unsupportedFields[T: c.WeakTypeTag] = collectFields(weakTypeOf[T]).filter(!isSupported(_))
  protected def permanentFields[T: c.WeakTypeTag] = supportedFields[T].filter(!isTransient(_))
  protected def transientFields[T: c.WeakTypeTag] = supportedFields[T].filter( isTransient)
  
  protected def size[T: c.WeakTypeTag](world: c.Expr[Playground]): Int = {
    assert(unsupportedFields.isEmpty, "")
    val toStore = permanentFields 
    toStore.map(length).foldLeft(0)( _ + _ )
  }

}
