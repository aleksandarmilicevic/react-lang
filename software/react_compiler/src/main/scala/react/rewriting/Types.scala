package react.rewriting

trait Types {
  self: ExplorableMacros =>
  import c.universe._
  
  /** check whether this is a supported type
   * TODO extends to <:< AnyVal */
  protected def isSupported(m: TermSymbol) = {
    import definitions._
    val t = m.typeSignature
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

  protected def length(t: Type): Int = {
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
  protected def length(s: TermSymbol): Int = length(s.typeSignature)
  
  protected def write(t: Type): TermName = {
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
  protected def write(s: TermSymbol): TermName = write(s.typeSignature)
  
  protected def read(t: Type): TermName = {
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
  protected def read(s: TermSymbol): TermName = read(s.typeSignature)

  protected def havoc(t: Type): Tree = {
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
  protected def havoc(s: TermSymbol): Tree = havoc(s.typeSignature)


}
