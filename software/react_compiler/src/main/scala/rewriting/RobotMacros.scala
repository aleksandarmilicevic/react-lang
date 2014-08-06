package react.rewriting

import scala.language.experimental.macros
//import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.blackbox.Context

import react.verification.World

class RobotMacros(val c: Context) {

  import c.universe._

  /** check whether this is a supported type
   * TODO extends to <:< AnyVal */
  private def isSupported(m: Symbol) = {
    import definitions._
    val t = m.tpe
    t =:= DoubleTpe ||
    t =:= FloatTpe ||
    t =:= CharTpe ||
    t =:= ByteTpe ||
    t =:= ShortTpe ||
    t =:= IntTpe ||
    t =:= LongTpe ||
    t =:= BooleanTpe ||
    t =:= UnitTpe
  }

  private def length(t: Type): Int = {
    import definitions._
    if (t =:= DoubleTpe)        8
    else if (t =:= FloatTpe)    4
    else if (t =:= CharTpe)     2
    else if (t =:= ByteTpe)     1
    else if (t =:= ShortTpe)    2
    else if (t =:= IntTpe)      4
    else if (t =:= LongTpe)     8
    else if (t =:= BooleanTpe)  1
    else if (t =:= UnitTpe)     0
    else sys.error("does not know the size of: " + showRaw(t))
  }
  private def length(s: Symbol): Int = length(s.tpe)
  
  private def write(t: Type): TermName = {
    import definitions._
    if (t =:= DoubleTpe)        TermName("putDouble")
    else if (t =:= FloatTpe)    TermName("putFloat")
    else if (t =:= CharTpe)     TermName("putChar")
    else if (t =:= ByteTpe)     TermName("putByte")
    else if (t =:= ShortTpe)    TermName("putShort")
    else if (t =:= IntTpe)      TermName("putInt")
    else if (t =:= LongTpe)     TermName("putLong")
    else if (t =:= BooleanTpe)  TermName("putBoolean")
    //UnitTpe disappeared ...
    else sys.error("does not know how to store: " + showRaw(t))
  }
  private def write(s: Symbol): TermName = write(s.tpe)
  
  private def read(t: Type): TermName = {
    import definitions._
    if (t =:= DoubleTpe)        TermName("getDouble")
    else if (t =:= FloatTpe)    TermName("getFloat")
    else if (t =:= CharTpe)     TermName("getChar")
    else if (t =:= ByteTpe)     TermName("getByte")
    else if (t =:= ShortTpe)    TermName("getShort")
    else if (t =:= IntTpe)      TermName("getInt")
    else if (t =:= LongTpe)     TermName("getLong")
    else if (t =:= BooleanTpe)  TermName("getBoolean")
    //UnitTpe disappeared ...
    else sys.error("does not know how to restore: " + showRaw(t))
  }
  private def read(s: Symbol): TermName = write(s.tpe)

  private def havoc(t: Type): Tree = {
    import definitions._
    if (t =:= DoubleTpe)        q"scala.util.Random.nextDouble()"
    else if (t =:= FloatTpe)    q"scala.util.Random.nextFloat()"
    else if (t =:= CharTpe)     q"scala.util.Random.nextChar()"
    else if (t =:= ByteTpe)     q"scala.util.Random.nextByte()"
    else if (t =:= ShortTpe)    q"scala.util.Random.nextShort()"
    else if (t =:= IntTpe)      q"scala.util.Random.nextInt()"
    else if (t =:= LongTpe)     q"scala.util.Random.nextLong()"
    else if (t =:= BooleanTpe)  q"scala.util.Random.nextBoolean()"
    //UnitTpe disappeared ...
    else sys.error("does not know how to havoc: " + showRaw(t))
  }
  private def havoc(s: Symbol): Tree = write(s.tpe)

  /** check if a symbol is transient:
   *  transient annotation might be a good idea to minimize the verification state space.
   */
  private def isTransient(m: Symbol) = {
    // http://stackoverflow.com/questions/17236066/scala-macros-checking-for-a-certain-annotation
    //m.accessed.annotations.exists( _.tpe =:= typeOf[scala.transient] )
    m.annotations.exists( _.tpe =:= typeOf[scala.transient] )
  }

  private def isShadow(m: Symbol) = {
    m.name.decodedName.toString startsWith "shadow_"
  }

  /** collect every declared variables (mutable field) */
  private def collectFields[T: c.TypeTag]: List[Symbol] = {
    //  http://docs.scala-lang.org/overviews/reflection/symbols-trees-types.html
    weakTypeOf[T].members.filter( m => m.isVar && !isShadow(m) )

//  // http://stackoverflow.com/questions/17223213/scala-macros-making-a-map-out-of-fields-of-a-class-in-scala
//  // http://meta.plasm.us/posts/2013/08/30/horrible-code/
//  val getters = weakTypeOf[T].declarations.collect {
//    case m: MethodSymbol if m.isAccessor => m
//  }
//  val setters =  weakTypeOf[T].declarations.collect {
//    case sym: MethodSymbol if sym.isSetter => m
//  }
//  val mutable = setters.map( set => {
//    val name = set.name.decodedName.toString
//    name dropRight 2 //remove '_='
//  })
//  getters.filter( get => {
//    val n = get.name.decodedName.toString
//    mutable contains n
//  })
  }
  
  private def fieldGetter(m: Symbol) = {
    val robot = Select(c.prefix.tree, newTermName("robot"))
    c.Expr(Select(robot, m.getter.name))
  }
  
  private def fieldSetter(m: Symbol) = {
    val robot = Select(c.prefix.tree, newTermName("robot"))
    c.Expr(Select(robot, m.setter.name))
  }

  private def supportedFields[T: c.WeakTypeTag] = collectFields[T].filter(isSupported)
  private def unsupportedFields[T: c.WeakTypeTag] = collectFields[T].filter(!isSupported)
  private def permanentFields[T: c.WeakTypeTag] = supportedFields[T].filter(!isTransient)
  private def transientFields[T: c.WeakTypeTag] = supportedFields[T].filter( isTransient)

  def toWord[T: c.WeakTypeTag](world: c.Expr[World]): c.Expr[Array[Byte]] = {
    val toStore = permanentFields
    val size = toStore.map(length).foldLeft(0)( _ + _ )
    val storing = for (f <- toStore) yield {
      val getter = fieldGetter(f)
      val writer = write(f)
      q"buffer.$writer($getter)"
    }
    val tree = q"""
    {
      val buffer = java.nio.ByteBuffer.allocate($size)
      ..$storing
      buffer.array()
    }
    """
    c.Expr[Array[Byte]](tree)
  }

  def fromWord[T: c.WeakTypeTag](world: c.Expr[World], state: c.Expr[Array[Byte]]): c.Expr[Unit] = {

    val restored = for (f <- permanentFields) yield {
      val setter = fieldSetter(f)
      val reader = read(f)
      q"$setter($reader(buffer))"
    }

    val havoced = for (f <- transientFields) yield {
      val setter = fieldSetter(f)
      val havoc = havoc(f)
      q"$setter($havoc)"
    }

    val tree = q"""
    {
      val buffer = java.nio.ByteBuffer.wrap($state)
      ..$restored
      ..$havoced
    }
    """

    c.Expr[Unit](tree)
  }

}
