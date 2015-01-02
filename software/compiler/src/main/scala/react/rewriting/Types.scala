package react.rewriting

import java.nio.ByteBuffer

trait Types {
  self: ExplorableMacros =>
  import c.universe._

  import definitions._

  trait TypeIO {
    def is(t: Type): Boolean
    def length: Int
    def worldAgnostic: Boolean
    def read(in: c.Expr[ByteBuffer], setter: Tree): Tree
    def write(out: c.Expr[ByteBuffer], getter: Tree): Tree
    def havoc(setter: Tree): Tree
  }

  val supportedType = List[TypeIO](
    new TypeIO {
      def is(t: Type) = t =:= UnitTpe
      def length: Int = 0
      def worldAgnostic: Boolean = true
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"()"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"()"
      def havoc(setter: Tree): Tree = q"()"
    },
    new TypeIO {
      def is(t: Type) = t =:= BooleanTpe
      def length: Int = 1
      def worldAgnostic: Boolean = true
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.get != 0)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"if ($getter) $out.put(1: Byte) else $out.put(0: Byte)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextBoolean())"
    },
    new TypeIO {
      def is(t: Type) = t =:= ByteTpe
      def length: Int = 1
      def worldAgnostic: Boolean = true
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.get)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.put($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextByte())"
    },
    new TypeIO {
      def is(t: Type) = t =:= CharTpe
      def length: Int = 2
      def worldAgnostic: Boolean = true
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getChar)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putChar($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextChar())"
    },
    new TypeIO {
      def is(t: Type) = t =:= ShortTpe
      def length: Int = 2
      def worldAgnostic: Boolean = true
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getShort)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putShort($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextShort())"
    },
    new TypeIO {
      def is(t: Type) = t =:= IntTpe
      def length: Int = 4
      def worldAgnostic: Boolean = true
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getInt)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putInt($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextInt())"
    },
    new TypeIO {
      def is(t: Type) = t =:= FloatTpe
      def length: Int = 4
      def worldAgnostic: Boolean = true
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getFloat)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putFloat($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextFloat())"
    },
    new TypeIO {
      def is(t: Type) = t =:= LongTpe
      def length: Int = 8
      def worldAgnostic: Boolean = true
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getLong)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putLong($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextLong())"
    },
    new TypeIO {
      def is(t: Type) = t =:= DoubleTpe
      def length: Int = 8
      def worldAgnostic: Boolean = true
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter($in.getDouble)"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.putDouble($getter)"
      def havoc(setter: Tree): Tree = q"$setter(scala.util.Random.nextDouble())"
    },
    new TypeIO {
      def is(t: Type) = t match {
        case TypeRef(ThisType(_), sym, List()) => showRaw(sym) == "scala.Symbol"
        case _ => false
      }
      def length: Int = 1
      def worldAgnostic: Boolean = true
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter(react.verification.SymbolCache.value($in.get))"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.put(react.verification.SymbolCache.idx($getter).toByte)"
      def havoc(setter: Tree): Tree = q"$setter(react.verification.SymbolCache.returnSome)"
    },
    new TypeIO {
      def is(t: Type) = t.toString == "react.robot.Orientation"
      def length: Int = 1
      def worldAgnostic: Boolean = true
      def read(in: c.Expr[ByteBuffer], setter: Tree): Tree = q"$setter(react.verification.OrientationCache.value($in.get))"
      def write(out: c.Expr[ByteBuffer], getter: Tree): Tree = q"$out.put(react.verification.OrientationCache.idx($getter).toByte)"
      def havoc(setter: Tree): Tree = q"$setter(react.verification.OrientationCache.returnSome)"
    }
  )
  
  object IsTuple {
    def unapply(t: Type): Option[List[Type]] = t match {
      case TypeRef(_, tRef, args) if showRaw(tRef) startsWith "scala.Tuple" => Some(args)
      case _ => None
    }
  }

  object IsImmutableCollection {
    def unapply(t: Type): Option[List[Type]] = t match {
      case TypeRef(_, tRef, args) =>
        val sr = showRaw(tRef)
        if (sr.startsWith("scala.collection.immutable") || 
            sr == "TypeName(\"List\")" || //TODO check prefix: scala.root
            sr == "TypeName(\"Set\")" || //TODO check prefix: scala.Predef
            sr == "TypeName(\"Map\")") { //TODO check prefix: scala.Predef
          Some(args)
        } else {
          None
        }
      case _ => None
    }
  }

  object IsOption {
    def unapply(t: Type): Option[Type] = t match {
      case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "scala.Option" => Some(arg)
      case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "scala.Some" => Some(arg)
      case TypeRef(_, tRef, List(arg)) if showRaw(tRef) == "scala.None" => Some(arg)
      case _ => None
    }
  }


  protected def cacheId(m: TermSymbol): TermName = TermName(m.name.toString + "__cache")

  protected def getCache(t: Type): Tree = {
    if (worldAgnostic(t)) {
      q"react.verification.MetaCache.getCache[$t]"
    } else {
      q"new react.verification.Cache[$t]"
    }
  }
  
  /** check whether this is a supported type */
  protected def isSupported(m: TermSymbol) = true

  /** check whether this is a natively supported type (or if it needs a cache) */
  protected def isNative(m: TermSymbol) = {
    val t = m.typeSignature
    supportedType.exists(_.is(t))
  }

  protected def worldAgnostic(t: Type): Boolean = {
    supportedType.find(_.is(t)) match {
      case Some(s) => s.worldAgnostic
      case None =>
        t match {
          case IsTuple(tps) => tps forall worldAgnostic
          case IsImmutableCollection(tps) => tps forall worldAgnostic
          case IsOption(t) => worldAgnostic(t)
          case _ => false
        }
    }
  }
  protected def worldAgnostic(m: TermSymbol): Boolean = {
    worldAgnostic(m.typeSignature)
  }

  protected def length(t: Type): Int =
    supportedType.find(_.is(t)).map(_.length).getOrElse(4)
  protected def length(s: TermSymbol): Int =
    length(s.typeSignature)
  
  protected def write(s: TermSymbol, out: c.Expr[ByteBuffer], getter: Tree): Tree = {
    val t = s.typeSignature
    supportedType.find(_.is(t)) match {
      case Some(tio) => tio.write(out, getter)
      case None => 
        val cid = cacheId(s)
        q"$out.putInt($cid.idx($getter))"
    }
  }

  protected def read(s: TermSymbol, in: c.Expr[ByteBuffer], setter: Tree): Tree = {
    val t = s.typeSignature
    supportedType.find(_.is(t)) match {
      case Some(tio) => tio.read(in, setter)
      case None =>
        val cid = cacheId(s)
        q"$setter($cid.value($in.getInt))"
    }
  }

  protected def havoc(t: Type, setter: Tree): Tree =
    supportedType.find(_.is(t)).get.havoc(setter)
  protected def havoc(s: TermSymbol, setter: Tree): Tree =
    havoc(s.typeSignature, setter)


}
